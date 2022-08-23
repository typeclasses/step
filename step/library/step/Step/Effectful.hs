{-# language UndecidableInstances #-}
{-# language DataKinds, KindSignatures, InstanceSigs, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# language ConstraintKinds, DataKinds, FlexibleContexts, KindSignatures, QuantifiedConstraints, TypeOperators #-}
{-# language DataKinds, StandaloneKindSignatures, FunctionalDependencies, FlexibleInstances #-}
{-# language DeriveAnyClass, DeriveFunctor, DerivingVia, GeneralizedNewtypeDeriving, EmptyCase, GADTs, AllowAmbiguousTypes #-}

module Step.Effectful where

import Step.Internal.Prelude hiding (evalState, runState, put, get, execState)

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

-- ⭕ General utilities for casting effects

castEff :: DispatchOf e1 ~ 'Dynamic => DispatchOf e2 ~ 'Dynamic =>
    (forall m1 m2 a'. e1 m1 a' -> e2 m2 a') -> Eff (e1 ': es) a -> Eff (e2 ': es) a
castEff f = interpret (\_ -> send . f) . swapEff . raise

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
    fail :: Eff es e -> act xs x es e a

instance Fallible Any where
    fail :: forall xs x e a es. Eff es e -> Any xs x es e a
    fail x = Any do
        e <- raise x
        send @(Step 'ReadWrite 'Imperfect xs x e) (StepFail e)

instance Fallible Query where
    fail :: forall xs x e a es. Eff es e -> Query xs x es e a
    fail x = Query do
        e <- raise x
        send @(Step 'ReadOnly 'Imperfect xs x e) (StepFail e)

instance Fallible Move where
    fail = Move . fail

instance Fallible Atom where
    fail = Atom . fail

instance Fallible AtomicMove where
    fail = AtomicMove . fail

instance Fallible a => Is Failure a where
    cast (Failure x) = fail x
