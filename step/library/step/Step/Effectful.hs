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

data Step (mo :: Mode) (xs :: Type) (x :: Type) :: Effect
  where
    Commit :: Positive Natural -> Step 'ReadWrite xs x m AdvanceResult
    Next :: Step mo xs x m (Maybe (Nontrivial xs x))
    Reset :: Step mo xs x m ()

data Mode = ReadOnly | ReadWrite

type instance DispatchOf (Step mo xs x) = 'Dynamic

runStepR :: forall xs x es a. '[Load xs x, Buffer 'CommitBuffer xs x] :>> es =>
    Eff (Step 'ReadOnly xs x ': es) a -> Eff es a
runStepR = reinterpret runStepHandler stepHandlerR

stepHandlerR :: forall xs x es. '[Load xs x, Buffer 'CommitBuffer xs x] :>> es =>
    EffectHandler (Step 'ReadOnly xs x) (Buffer 'ViewBuffer xs x ': es)
stepHandlerR _env = \case
    Reset -> getBufferSeq @'CommitBuffer @xs @x >>= putBufferSeq @'ViewBuffer @xs @x
    Next -> runStepNext

runStepRW :: forall xs x es a. '[Load xs x, Buffer 'CommitBuffer xs x] :>> es => NT.DropOperation xs x
    -> Eff (Step 'ReadWrite xs x ': es) a -> Eff es a
runStepRW dropOp = reinterpret runStepHandler (stepHandlerRW dropOp)

stepHandlerRW :: forall xs x es. '[Load xs x, Buffer 'CommitBuffer xs x] :>> es => NT.DropOperation xs x
    -> EffectHandler (Step 'ReadWrite xs x) (Buffer 'ViewBuffer xs x ': es)
stepHandlerRW dropOp _env = \case
    Reset -> getBufferSeq @'CommitBuffer @xs @x >>= putBufferSeq @'ViewBuffer @xs @x
    Next -> runStepNext
    Commit n -> runStepCommit @xs @x dropOp n

runStepHandler :: forall xs x es a. Buffer 'CommitBuffer xs x :> es => Eff (Buffer 'ViewBuffer xs x ': es) a -> Eff es a
runStepHandler a = getBufferSeq @'CommitBuffer @xs @x >>= \b -> runBuffer' @'ViewBuffer @xs @x b a

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

-- ⭕ The Walk kind

-- | A "walk" is a collection of "steps"
type Walk = Type -> Type -> [Effect] -> Type -> Type -> Type

-- ⭕ Simple walks that are just a newtype for an Eff with a Step effect

type Any :: Walk
type Query :: Walk
type Sure :: Walk
type SureQuery :: Walk

-- | The most general of the walks
data Any xs x es e a = forall es'.
    Subset (Step 'ReadWrite xs x ': Step 'ReadOnly xs x ': Error e ': es) es' =>
    Any (Eff es' a)
    -- deriving newtype (Functor, Applicative, Monad)

-- | Like 'Any', but cannot move the cursor
data Query xs x es e a = forall es'.
    Subset (Step 'ReadOnly xs x ': Error e ': es) es' =>
    Query (Eff es' a)
    -- deriving newtype (Functor, Applicative, Monad)

-- | Always succeeds
data Sure xs x es e a =
    forall es'.
    ( Step 'ReadWrite xs x :> es'
    , Step 'ReadOnly xs x :> es'
    , es :>> es'
    ) =>
    Sure (Eff es' a)
    -- deriving newtype (Functor, Applicative, Monad)

-- | Always succeeds, does not move the cursor
data SureQuery xs x es e a =
    forall es'.
    ( Step 'ReadOnly xs x :> es'
    , es :>> es'
    ) =>
    SureQuery (Eff es' a)
    -- deriving newtype (Functor, Applicative, Monad)

-- ⭕ Walks defined in terms of others

type Atom :: Walk
type Move :: Walk
type AtomicMove :: Walk

-- | Fails noncommittally; see 'try'
newtype Atom xs x es e a = Atom (Query xs x es e (Sure xs x es e a))
    -- deriving stock (Functor)

-- | Always moves the cursor
newtype Move xs x es e a = Move (Any xs x es e a)
    -- deriving stock (Functor)

-- | Always moves the cursor, is atomic
newtype AtomicMove xs x es e a = AtomicMove (Atom xs x es e a)
    -- deriving stock (Functor)

-- instance (TypeError ('Text "Atom cannot be Applicative because (<*>) would not preserve atomicity")) => Applicative (Atom xs x e m) where
--     pure = error "unreachable"
--     (<*>) = error "unreachable"

-- instance (TypeError ('Text "Move cannot be Applicative because 'pure' would not move the cursor")) =>
--     Applicative (Move xs x e es)
--   where
--     pure = error "unreachable"
--     (<*>) = error "unreachable"

-- instance (TypeError ('Text "AtomicMove cannot be Applicative because 'pure' would not move the cursor and (<*>) would not preserve atomicity")) => Applicative (AtomicMove xs x e m) where
--     pure = error "unreachable"
--     (<*>) = error "unreachable"

-- ⭕ Failure

type Fail :: Walk

newtype Fail xs x es e a = Fail (Eff (Error e ': es) Void)
    deriving stock (Functor)

-- ⭕ Class for action subtype relationship

class Is (act1 :: Walk) (act2 :: Walk) where
    cast :: act1 xs x es e a -> act2 xs x es e a

castTo :: forall act2 act1 xs x es e a. Is act1 act2 => act1 xs x es e a -> act2 xs x es e a
castTo = cast @act1 @act2

-- ⭕ Everything is itself

instance {-# overlappable #-} Is a a where
    cast = id

-- ⭕ Casting out of read-only

castRW :: forall xs x es a. Step 'ReadOnly xs x :> es => Eff es a -> Eff (Step 'ReadWrite xs x ': es) a
castRW = raise

instance Is SureQuery Sure where
    -- cast :: forall xs x es e a. SureQuery xs x es e a -> Sure xs x es e a
    -- cast (SureQuery (x :: Eff es' a)) = Sure (inject x :: Eff (Step 'ReadWrite xs x ': es') a)

instance Is Query Any where
    -- cast (Query x) = Any (raise x)

-- ⭕ Casting out of sureness

instance Is SureQuery Query where
    -- cast (SureQuery x) = Query (raise x)

instance Is Sure Any where
    -- cast (Sure x) = Any (raise x)
