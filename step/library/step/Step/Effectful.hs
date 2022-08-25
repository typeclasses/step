{-# language UndecidableInstances #-}
{-# language DataKinds, KindSignatures, InstanceSigs, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# language ConstraintKinds, DataKinds, FlexibleContexts, KindSignatures, QuantifiedConstraints, TypeOperators #-}
{-# language DataKinds, StandaloneKindSignatures, FunctionalDependencies, FlexibleInstances #-}
{-# language DeriveAnyClass, DeriveFunctor, DerivingVia, GeneralizedNewtypeDeriving, EmptyCase, GADTs, AllowAmbiguousTypes #-}

module Step.Effectful where

import Step.Internal.Prelude hiding (evalState, runState, put, get, execState, ask)

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as NT

import Positive.Unsafe (Positive (PositiveUnsafe))
import qualified Positive
import qualified Positive.Math as Positive
import qualified Signed

import Effectful
import Effectful.Dispatch.Static
import Effectful.Error.Static
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Effectful.Reader.Static

import TypeLits (TypeError, ErrorMessage (Text))

import Data.Primitive.PrimArray
import Effectful.Internal.Env
import Effectful.Internal.Monad

import qualified Monad

-- ⭕ The Buffer effect

data Buffer (bt :: BufferType) (xs :: Type) (x :: Type) :: Effect

data BufferType = CommitBuffer | ViewBuffer

type instance DispatchOf (Buffer bt xs x) = 'Static 'NoSideEffects

newtype instance StaticRep (Buffer bt xs x) = BufferSeq{ bufferSeq :: Seq (Nontrivial xs x) }
    deriving newtype (Semigroup, Monoid)

instance IsList (StaticRep (Buffer bt xs x)) where
    type Item (StaticRep (Buffer bt xs x)) = Nontrivial xs x
    fromList = BufferSeq . fromList
    toList = toList . bufferSeq

runBuffer :: forall bt xs x es a. Eff (Buffer bt xs x ': es) a -> Eff es a
runBuffer = runBuffer' mempty

runBuffer' :: forall bt xs x es a. Seq (Nontrivial xs x) -> Eff (Buffer bt xs x ': es) a -> Eff es a
runBuffer' = evalStaticRep . BufferSeq

getBufferSeq :: forall bt xs x es. Buffer bt xs x :> es => Eff es (Seq (Nontrivial xs x))
getBufferSeq = getStaticRep @(Buffer bt xs x) <&> bufferSeq

putBufferSeq :: forall bt xs x es. Buffer bt xs x :> es => Seq (Nontrivial xs x) -> Eff es ()
putBufferSeq = putStaticRep @(Buffer bt xs x) . BufferSeq

feedBuffer :: forall bt xs x es. Buffer bt xs x :> es => Nontrivial xs x -> Eff es ()
feedBuffer x = getBufferSeq @bt @xs @x >>= \xs -> putBufferSeq @bt @xs @x (xs :|> x)

returnToBuffer :: forall bt xs x es. Buffer bt xs x :> es => Nontrivial xs x -> Eff es ()
returnToBuffer x = getBufferSeq @bt >>= \xs -> putBufferSeq @bt (x :<| xs)

takeBufferChunk :: forall bt xs x es. Buffer bt xs x :> es => Eff es (Maybe (Nontrivial xs x))
takeBufferChunk = getBufferSeq @bt >>= \case
    Empty -> return Nothing
    y :<| ys -> putBufferSeq @bt ys $> Just y

dropFromBuffer :: forall bt xs x es. Buffer bt xs x :> es => NT.DropOperation xs x -> Positive Natural -> Eff es AdvanceResult
dropFromBuffer NT.DropOperation{ NT.drop } = fix \r n -> getBufferSeq @bt >>= \case
    Empty -> return YouCanNotAdvance{ shortfall = n }
    x :<| xs -> case drop n x of
        NT.DropAll -> putBufferSeq @bt xs $> AdvanceSuccess
        NT.DropPart{ NT.dropRemainder } -> putBufferSeq @bt (dropRemainder :<| xs) $> AdvanceSuccess
        NT.DropInsufficient{ NT.dropShortfall } -> putBufferSeq @bt xs *> r dropShortfall

data AdvanceResult =
    AdvanceSuccess
  | YouCanNotAdvance{ shortfall :: Positive Natural }

-- ⭕ The Load effect

data Load (xs :: Type) (x :: Type) :: Effect
  where
    Load :: Load xs x m (Maybe (Nontrivial xs x))

type instance DispatchOf (Load xs x) = 'Dynamic

load :: Load xs x :> es => Eff es (Maybe (Nontrivial xs x))
load = send Load

loadFromList :: [Nontrivial xs x] -> Eff (Load xs x ': es) a-> Eff es (a, [Nontrivial xs x])
loadFromList xs = reinterpret (runState xs) loadFromListHandler

loadFromListHandler :: State [Nontrivial xs x] :> es => EffectHandler (Load xs x) (es)
loadFromListHandler _env Load = get >>= \case
    [] -> return Nothing
    x : xs -> put xs $> Just x

-- ⭕ The Step effect

data Step (mo :: Mode) (p :: Perfection) (xs :: Type) (x :: Type) (e :: Type) :: Effect
  where
    StepCommit :: Positive Natural -> Step 'ReadWrite p xs x m e AdvanceResult
    StepNext :: Step mo p xs x m e (Maybe (Nontrivial xs x))
    StepReset :: Step mo p xs x m e ()
    StepFail :: e -> Step mo 'Imperfect xs x e m a

data Perfection = Perfect | Imperfect

data Mode = ReadOnly | ReadWrite

type instance DispatchOf (Step mo p xs x e) = 'Dynamic

castStepMode :: Step 'ReadOnly p xs x e m a -> Step 'ReadWrite p xs x e m' a
castStepMode = \case
    StepNext -> StepNext
    StepReset -> StepReset
    StepFail x -> StepFail x

castStepPerfection :: Step mo 'Perfect xs x e m a -> Step mo 'Imperfect xs x e m' a
castStepPerfection = \case
    StepNext -> StepNext
    StepReset -> StepReset
    StepCommit x -> StepCommit x

castStepDual :: Step 'ReadOnly 'Perfect xs x e m a -> Step 'ReadWrite 'Imperfect xs x e m' a
castStepDual = \case
    StepNext -> StepNext
    StepReset -> StepReset

tryStep :: Step mo 'Imperfect xs x e m a -> Maybe (Step mo 'Perfect xs x e' m' a)
tryStep = \case
    StepFail _ -> Nothing
    StepNext -> Just StepNext
    StepReset -> Just StepReset
    StepCommit n -> Just (StepCommit n)

runStep :: forall xs x e es a. '[Load xs x, Buffer 'CommitBuffer xs x, Error e] :>> es => NT.DropOperation xs x
    -> Eff (Step 'ReadWrite 'Imperfect xs x e ': es) a -> Eff es a
runStep dropOp = reinterpret runStepHandler (stepHandler dropOp)

runStepHandler :: forall xs x es a. Buffer 'CommitBuffer xs x :> es => Eff (Buffer 'ViewBuffer xs x ': es) a -> Eff es a
runStepHandler a = getBufferSeq @'CommitBuffer @xs @x >>= \b -> runBuffer' @'ViewBuffer @xs @x b a

stepHandler :: forall xs x es e. '[Load xs x, Buffer 'CommitBuffer xs x, Error e] :>> es => NT.DropOperation xs x
    -> EffectHandler (Step 'ReadWrite 'Imperfect xs x e) (Buffer 'ViewBuffer xs x ': es)
stepHandler dropOp _env = \case
      StepFail e -> throwError e
      StepReset -> getBufferSeq @'CommitBuffer @xs @x >>= putBufferSeq @'ViewBuffer @xs @x
      StepNext -> runStepNext
      StepCommit n -> runStepCommit @xs @x dropOp n

runStepNext :: forall xs x es. '[Load xs x, Buffer 'ViewBuffer xs x, Buffer 'CommitBuffer xs x] :>> es => Eff es (Maybe (Nontrivial xs x))
runStepNext = takeBufferChunk @'ViewBuffer >>= \case
    Just x -> return (Just x)
    Nothing -> bufferMore @xs @x >>= \case{ True -> takeBufferChunk @'ViewBuffer; False -> return Nothing }

runStepCommit :: forall xs x es. '[Load xs x, Buffer 'ViewBuffer xs x, Buffer 'CommitBuffer xs x] :>> es => NT.DropOperation xs x -> Positive Natural -> Eff es AdvanceResult
runStepCommit dropOp n = dropFromBuffer @'CommitBuffer @xs @x dropOp n >>= \case
    r@AdvanceSuccess -> return r
    r@YouCanNotAdvance{ shortfall = n' } -> bufferMore @xs @x >>= \case{ True -> dropFromBuffer @'CommitBuffer @xs @x dropOp n'; False -> return r }

bufferMore :: forall xs x es. '[Load xs x, Buffer 'ViewBuffer xs x, Buffer 'CommitBuffer xs x] :>> es => Eff es Bool
bufferMore = load @xs @x >>= \case
    Nothing -> return False
    Just x -> do{ feedBuffer @'CommitBuffer x; feedBuffer @'ViewBuffer x; return True }

-- ⭕ The Action kind

type Action = Type -> Type -> [Effect] -> Type -> Type -> Type

-- ⭕ Simple actions that are just a newtype for an action with a Step effect

type Any :: Action
type Query :: Action
type Sure :: Action
type SureQuery :: Action

-- | The most general of the actions
newtype Any xs x es e a = Any (Eff (Step 'ReadWrite 'Imperfect xs x e ': es) a)
    deriving newtype (Functor, Applicative, Monad)

-- | Like 'Any', but cannot move the cursor
newtype Query xs x es e a = Query (Eff (Step 'ReadOnly 'Imperfect xs x e ': es) a)
    deriving newtype (Functor, Applicative, Monad)

-- | Always succeeds
newtype Sure xs x es e a = Sure (Eff (Step 'ReadWrite 'Perfect xs x e ': es) a)
    deriving newtype (Functor, Applicative, Monad)

-- | Always succeeds, does not move the cursor
newtype SureQuery xs x es e a = SureQuery (Eff (Step 'ReadOnly 'Perfect xs x e ': es) a)
    deriving newtype (Functor, Applicative, Monad)

-- ⭕ Actions defines in terms of others

type Atom :: Action
type Move :: Action
type AtomicMove :: Action

-- | Fails noncommittally; see 'try'
newtype Atom xs x es e a = Atom (Query xs x es e (Sure xs x es e a))
    deriving stock (Functor)

-- | Always moves the cursor
newtype Move xs x es e a = Move (Any xs x es e a)
    deriving stock (Functor)

-- | Always moves the cursor, is atomic
newtype AtomicMove xs x es e a = AtomicMove (Atom xs x es e a)
    deriving stock (Functor)

instance (TypeError ('Text "Atom cannot be Applicative because (<*>) would not preserve atomicity")) => Applicative (Atom xs x e m) where
    pure = error "unreachable"
    (<*>) = error "unreachable"

instance (TypeError ('Text "Move cannot be Applicative because 'pure' would not move the cursor")) =>
    Applicative (Move xs x e es)
  where
    pure = error "unreachable"
    (<*>) = error "unreachable"

instance (TypeError ('Text "AtomicMove cannot be Applicative because 'pure' would not move the cursor and (<*>) would not preserve atomicity")) => Applicative (AtomicMove xs x e m) where
    pure = error "unreachable"
    (<*>) = error "unreachable"

-- ⭕ Failure

newtype Failure xs x es e a = Failure (Eff es e)
    deriving stock (Functor)

instance (TypeError ('Text "Failure cannot be Applicative because 'pure' would succeed")) => Applicative (Failure xs x e m) where
    pure = error "unreachable"
    (<*>) = error "unreachable"

-- ⭕ The Nil effect

type Nil :: Effect

data Nil m a = Nil

type instance DispatchOf Nil = 'Dynamic

runNil :: Eff (Nil ': es) a -> Eff es (Maybe a)
runNil = fmap (either (\() -> Nothing) Just) . reinterpret runErrorNoCallStack \_ -> \case{ Nil -> throwError () }

-- ⭕ General utilities for casting effects

castEff :: forall e1 e2 es a. DispatchOf e1 ~ 'Dynamic => DispatchOf e2 ~ 'Dynamic =>
    (forall m1 m2 a'. e1 m1 a' -> e2 m2 a') -> Eff (e1 ': es) a -> Eff (e2 ': es) a
castEff f = interpret (\_ -> send @e2 . f) . swapEff . raise

tryEff :: forall e1 e2 es a. DispatchOf e1 ~ 'Dynamic => DispatchOf e2 ~ 'Dynamic =>
    (forall m1 m2 a'. e1 m1 a' -> Maybe (e2 m2 a')) -> Eff (e1 ': es) a -> Eff (e2 ': es) (Maybe a)
tryEff f = reinterpret runNil (\_ -> maybe (send Nil) (raise . send @e2 @(e2 ': es)) . f) . swapEff . raise

-- https://github.com/haskell-effectful/effectful/discussions/91#discussioncomment-3451935
swapEff :: Eff (e1 : e2 : es) a -> Eff (e2 : e1 : es) a
swapEff m = unsafeEff $ \es -> unEff m =<< swapEnv es
  where
    swapEnv :: Env (e1 : e2 : es) -> IO (Env (e2 : e1 : es))
    swapEnv (Env offset refs0 storage) = do
      let size = sizeofPrimArray refs0 - offset
      mrefs <- newPrimArray size
      copyPrimArray mrefs 4 refs0 (offset + 4) (size - 4)
      writePrimArray mrefs 0 $ indexPrimArray refs0 2
      writePrimArray mrefs 1 $ indexPrimArray refs0 3
      writePrimArray mrefs 2 $ indexPrimArray refs0 0
      writePrimArray mrefs 3 $ indexPrimArray refs0 1
      refs <- unsafeFreezePrimArray mrefs
      pure $ Env 0 refs storage

-- ⭕ General utilities for casting effects

class Is (act1 :: Action) (act2 :: Action) where
    cast :: act1 xs x es e a -> act2 xs x es e a

castTo :: forall act2 act1 xs x es e a. Is act1 act2 => act1 xs x es e a -> act2 xs x es e a
castTo = cast @act1 @act2

-- ⭕ Everything is itself

instance {-# overlappable #-} Is a a where
    cast = id

-- ⭕ Casting actions via casting steps

instance Is SureQuery Sure where
    cast (SureQuery x) = Sure (castEff castStepMode x)

instance Is Query Any where
    cast (Query x) = Any (castEff castStepMode x)

instance Is SureQuery Query where
    cast (SureQuery x) = Query (castEff castStepPerfection x)

instance Is Sure Any where
    cast (Sure x) = Any (castEff castStepPerfection x)

instance Is SureQuery Any where
    cast (SureQuery x) = Any (castEff castStepDual x)

-- ⭕ Casting to Atom

instance Is SureQuery Atom where
    cast = Atom . fmap return . castTo @Query

instance Is Query Atom where
    cast = Atom . fmap return

instance Is Sure Atom where
    cast = Atom . return

-- ⭕ Casting out of atomicity

instance Is Atom Any where
    cast (Atom x) = Monad.join (cast @Sure @Any <$> cast @Query @Any x)

instance Is AtomicMove Move where
    cast (AtomicMove x) = Move (cast @Atom @Any x)

-- ⭕ Casting out of movement

instance Is Move Any where
    cast (Move x) = x

instance Is AtomicMove Atom where
    cast (AtomicMove x) = x

-- ⭕ Casting out of both atomicity and movement

instance Is AtomicMove Any where
    cast = cast . castTo @Move

-- ⭕ Casting out of failure

class Fallible (act :: Action) where
    liftFailEffect :: Eff es e -> act xs x es e a

instance Fallible Any where
    liftFailEffect :: forall xs x e a es. Eff es e -> Any xs x es e a
    liftFailEffect x = Any do
        e <- raise x
        send @(Step 'ReadWrite 'Imperfect xs x e) (StepFail e)

instance Fallible Query where
    liftFailEffect :: forall xs x e a es. Eff es e -> Query xs x es e a
    liftFailEffect x = Query do
        e <- raise x
        send @(Step 'ReadOnly 'Imperfect xs x e) (StepFail e)

instance Fallible Move where
    liftFailEffect = Move . liftFailEffect

instance Fallible Atom where
    liftFailEffect = Atom . liftFailEffect

instance Fallible AtomicMove where
    liftFailEffect = AtomicMove . liftFailEffect

instance Fallible a => Is Failure a where
    cast (Failure x) = liftFailEffect x

-- ⭕

-- | The type @a >> b@ is type of the expression @a >> b@.
--
-- This function is mostly not commutative (@a >> b@ is not the same as @b >> a@) because whether an atomic action'atomicity is preserved depends on the order of the composition in some cases.

type family (act1 :: Action) >> (act2 :: Action) :: Action
  where

    -- When failure is first, the second step is irrelevant.
    Failure >> k = Failure

    -- When failure is second, sureness and atomicity are lost.
    Sure >> Failure = Any
    SureQuery >> Failure = Query
    Atom >> Failure = Any
    AtomicMove >> Failure = Move
    k >> Failure = k

    -- Joining with SureQuery has no effect on the type
    SureQuery >> k = k
    k >> SureQuery = k

    -- Properties other than atomicity are closed under composition.
    Query >> Query = Query
    Sure >> Sure = Sure
    -- (Move >> Move = Move) is covered later below.

    -- When an atomic step is followed by an infallible step, atomicity is preserved.
    Atom >> Sure = Atom
    AtomicMove >> Sure = AtomicMove
    -- (>> SureQuery) has already been covered above.

    -- When an atomic step is preceded by a query, atomicity is preserved.
    Query >> Atom = Atom
    Query >> Sure = Atom
    Query >> AtomicMove = AtomicMove
    -- (SureQuery >>) has already been covered above.

    -- Movement of a part implies movement of the whole.
    k >> Move = Move
    Move >> k = Move

    -- When AtomicMove loses atomicity, it degrades to Move.
    AtomicMove >> _ = Move
    _ >> AtomicMove = Move

    -- All other combinations degrade to Any.
    _ >> _ = Any

-- ⭕

class Join (act1 :: Action) (act2 :: Action) where
    join :: act1 xs x es e (act2 xs x es e a) -> (act1 >> act2) xs x es e a

cast2 :: forall act2 act1 f xs x es e a. Is act1 act2 => Functor f => f (act1 xs x es e a) -> f (act2 xs x es e a)
cast2 = fmap (castTo @act2)

-- ⭕

class Is act2 act1 => AssumeMovement (act1 :: Action) (act2 :: Action) | act1 -> act2 where
    assumeMovement :: act1 xs x es e a -> act2 xs x es e a

instance AssumeMovement Any Move where
    assumeMovement = Move

instance AssumeMovement Atom AtomicMove where
    assumeMovement = AtomicMove

-- ⭕

instance Join Any Any where
    join = Monad.join
instance Join Any Atom where
    join = join @Any @Any . cast2 @Any
instance Join Any AtomicMove where
    join = assumeMovement . join @Any @Any . cast2 @Any
instance Join Any Failure where
    join = join @Any @Any . cast2 @Any
instance Join Any Move where
    join = assumeMovement . join @Any @Any . cast2 @Any
instance Join Any Query where
    join = join @Any @Any . cast2 @Any
instance Join Any SureQuery where
    join = join @Any @Any . cast2 @Any
instance Join Any Sure where
    join = join @Any @Any . cast2 @Any

instance Join Atom Any where
    join = join @Any @Any . castTo @Any
instance Join Atom Atom where
    join = join @Any @Any . castTo @Any . cast2 @Any
instance Join Atom AtomicMove where
    join = assumeMovement . join @Any @Any . castTo @Any . cast2 @Any
instance Join Atom Failure where
    join = join @Any @Any . castTo @Any . cast2 @Any
instance Join Atom Move where
    join = assumeMovement . join @Any @Any . castTo @Any . cast2 @Any
instance Join Atom Query where
    join = castTo @Any . join @Any @Any . castTo @Any . cast2 @Any
instance Join Atom SureQuery where
    join = Atom . fmap (join @Sure @SureQuery) . (\(Atom q) -> q)
instance Join Atom Sure where
    join = Atom . fmap (join @Sure @Sure) . (\(Atom q) -> q)

instance Join AtomicMove Any where
    join = assumeMovement . join . castTo @Atom
instance Join AtomicMove Atom where
    join = assumeMovement . join . castTo @Atom
instance Join AtomicMove AtomicMove where
    join = join . castTo @Atom
instance Join AtomicMove Failure where
    join = assumeMovement . join . castTo @Atom
instance Join AtomicMove Move where
    join = join . castTo @Atom
instance Join AtomicMove Query where
    join = assumeMovement . join . castTo @Atom
instance Join AtomicMove SureQuery where
    join = assumeMovement . join . castTo @Atom
instance Join AtomicMove Sure where
    join = assumeMovement . join . castTo @Atom

instance Join Failure Any where
    join (Failure f) = Failure f
instance Join Failure Atom where
    join (Failure f) = Failure f
instance Join Failure AtomicMove where
    join (Failure f) = Failure f
instance Join Failure Failure where
    join (Failure f) = Failure f
instance Join Failure Move where
    join (Failure f) = Failure f
instance Join Failure Query where
    join (Failure f) = Failure f
instance Join Failure Sure where
    join (Failure f) = Failure f
instance Join Failure SureQuery where
    join (Failure f) = Failure f

instance Join Move Any where
    join = assumeMovement . join . castTo @Any
instance Join Move Atom where
    join = assumeMovement . join . castTo @Any
instance Join Move AtomicMove where
    join = join . castTo @Any
instance Join Move Failure where
    join = assumeMovement . join . castTo @Any
instance Join Move Move where
    join = join . castTo @Any
instance Join Move Query where
    join = assumeMovement . join . castTo @Any
instance Join Move SureQuery where
    join = assumeMovement . join . castTo @Any
instance Join Move Sure where
    join = assumeMovement . join . castTo @Any

instance Join Query Any where
    join = join @Any @Any . castTo @Any
instance Join Query Atom where
    join = Atom . join . fmap (\(Atom q) -> q)
instance Join Query AtomicMove where
    join = assumeMovement . join @Query @Atom . cast2 @Atom
instance Join Query Failure where
    join = join @Query @Query . cast2 @Query
instance Join Query Move where
    join = assumeMovement . join @Query @Any . cast2 @Any
instance Join Query Query where
    join = Monad.join
instance Join Query SureQuery where
    join = join @Query @Query . cast2 @Query
instance Join Query Sure where
    join = Atom

instance Join Sure Any where
    join = join @Any @Any . castTo @Any
instance Join Sure Atom where
    join = castTo @Any . join @Any @Any . castTo @Any . cast2 @Any
instance Join Sure AtomicMove where
    join = assumeMovement . join @Any @Any . castTo @Any . cast2 @Any
instance Join Sure Failure where
    join = join @Any @Any . castTo @Any . cast2 @Any
instance Join Sure Move where
    join = assumeMovement . join @Any @Any . castTo @Any . cast2 @Any
instance Join Sure Query where
    join = castTo @Any . join @Any @Any . castTo @Any . cast2 @Any
instance Join Sure SureQuery where
    join = join @Sure @Sure . cast2 @Sure
instance Join Sure Sure where
    join = Monad.join

instance Join SureQuery Any where
    join = join @Any @Any . castTo @Any
instance Join SureQuery Atom where
    join = Atom . join @SureQuery @Query . fmap (\(Atom q) -> q)
instance Join SureQuery AtomicMove where
    join = assumeMovement . join . cast2 @Atom
instance Join SureQuery Failure where
    join = join @Query @Failure . castTo @Query
instance Join SureQuery Move where
    join = assumeMovement . join. cast2 @Any
instance Join SureQuery Query where
    join = join @Query @Query . castTo @Query
instance Join SureQuery Sure where
    join = join @Sure @Sure . castTo @Sure
instance Join SureQuery SureQuery where
    join = Monad.join

-- ⭕

class Atomic (act :: Action) (try :: Action) | act -> try where
    try :: act xs x es e a -> try xs x es e (Maybe a)

instance Atomic Atom Sure where
    try (Atom x) = castTo @Sure (try @Query x) >>= maybe (return Nothing) (fmap Just)

instance Atomic AtomicMove Sure where
    try = try @Atom . castTo @Atom

instance Atomic Query SureQuery where
    try :: forall xs x es e a. Query xs x es e a -> SureQuery xs x es e (Maybe a)
    try (Query q) = SureQuery (tryEff tryStep q)

-- ⭕

infixl 1 `bindAction`
bindAction :: (Join act1 act2, Functor (act1 xs x es e)) => act1 >> act2 ~ act3 => act1 xs x es e a -> (a -> act2 xs x es e b) -> act3 xs x es e b
bindAction x f = join (fmap f x)

-- ⭕

class Trivial (act :: Action) where
    trivial :: a -> act xs x e m a

instance Trivial Any where
    trivial x = Any (return x)

instance Trivial Query where
    trivial x = Query (return x)

instance Trivial Sure where
    trivial x = Sure (return x)

instance Trivial SureQuery where
    trivial x = SureQuery (return x)

instance Trivial Atom where
    trivial x = Atom (Query (return (trivial x)))

-- ⭕

commit :: forall xs x es e. Positive Natural -> AtomicMove xs x es e ()
commit n = AtomicMove $ Atom $ Query $ return $ Sure $ void $ send @(Step 'ReadWrite 'Perfect xs x e) $ StepCommit n

fail :: Reader e :> es => Failure xs x es e a
fail = Failure ask

takeCharMaybe :: Reader e :> es => NT.LeftViewOperation xs x -> Sure xs x es e (Maybe x)
takeCharMaybe lview = try (takeChar lview)

takeChar :: Reader e :> es => NT.LeftViewOperation xs x -> AtomicMove xs x es e x
takeChar lview = nextChar lview `bindAction` \x -> commit one $> x

nextChar :: Reader e :> es => NT.LeftViewOperation xs x -> Query xs x es e x
nextChar lview = nextCharMaybe lview `bindAction` maybe (castTo @Query fail) return

nextMaybe :: SureQuery xs x es e (Maybe (Nontrivial xs x))
nextMaybe = reset `bindAction` \() -> nextMaybe'

-- | Like 'nextMaybe', but doesn't reset first
nextMaybe' :: forall xs x es e. SureQuery xs x es e (Maybe (Nontrivial xs x))
nextMaybe' = SureQuery $ send @(Step 'ReadOnly 'Perfect xs x e) StepNext

next :: Reader e :> es => Query xs x es e (Nontrivial xs x)
next = nextMaybe `bindAction` maybe (castTo @Query fail) return

-- | Like 'next', but doesn't reset first
next' :: Reader e :> es => Query xs x es e (Nontrivial xs x)
next' = nextMaybe' `bindAction` maybe (castTo @Query fail) return

takeNext :: Reader e :> es => AtomicMove xs x es e (Nontrivial xs x)
takeNext = next `bindAction` \xs -> commit (NT.length xs) $> xs

takeNextMaybe :: Reader e :> es => Sure xs x es e (Maybe (Nontrivial xs x))
takeNextMaybe = try takeNext

nextCharMaybe :: NT.LeftViewOperation xs x -> SureQuery xs x es e (Maybe x)
nextCharMaybe NT.LeftViewOperation{ NT.leftView } =
    nextMaybe <&> fmap @Maybe (NT.popItem . view leftView)

satisfyJust :: Reader e :> es => NT.LeftViewOperation xs x -> (x -> Maybe a) -> AtomicMove xs x es e a
satisfyJust lview ok = nextCharMaybe lview `bindAction` \x -> case x >>= ok of Nothing -> castTo fail; Just y -> commit one $> y

skip0 :: Reader e :> es => Natural -> Any xs x es e ()
skip0 = maybe (return ()) (castTo @Any . skip)  . preview Positive.refine

skip :: Reader e :> es => Positive Natural -> Move xs x es e ()
skip n = next `bindAction` \x ->
    case Positive.minus (NT.length x) n of
        Signed.Minus n' ->
            commit (NT.length x) `bindAction` \_ -> skip n'
        _ -> castTo @Move (commit n)

skipAtomically0 :: Reader e :> es => Natural -> Atom xs x es e ()
skipAtomically0 = maybe (trivial ()) (castTo @Atom . skipAtomically)  . preview Positive.refine

skipAtomically :: Reader e :> es => Positive Natural -> AtomicMove xs x es e ()
skipAtomically n = ensureAtLeast n `bindAction` \() -> commit n

ensureAtLeast :: Reader e :> es => Positive Natural -> Query xs x es e ()
ensureAtLeast = \n -> castTo @Query reset `bindAction` \() -> go n
  where
    go :: Reader e :> es => Positive Natural -> Query xs x es e ()
    go n = next' `bindAction` \x ->
        case Positive.minus n (NT.length x) of
            Signed.Plus n' -> go n'
            _ -> return ()

atEnd :: SureQuery xs x es e Bool
atEnd = reset `bindAction` \() -> nextMaybe' <&> isNothing

end :: Reader e :> es => Query xs x es e ()
end = atEnd `bindAction` \e -> if e then trivial () else castTo @Query fail

reset :: forall xs x es e. SureQuery xs x es e ()
reset = SureQuery $ send @(Step 'ReadOnly 'Perfect xs x e) $ StepReset

-- ⭕

one :: Positive Natural
one = PositiveUnsafe 1
