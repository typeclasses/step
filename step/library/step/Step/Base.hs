{-# language UndecidableInstances #-}
{-# language DataKinds, KindSignatures, InstanceSigs, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# language ConstraintKinds, DataKinds, FlexibleContexts, KindSignatures, QuantifiedConstraints, TypeOperators #-}
{-# language DataKinds, StandaloneKindSignatures, FunctionalDependencies, FlexibleInstances, BlockArguments, LambdaCase #-}
{-# language DeriveAnyClass, DeriveFunctor, DerivingVia, GeneralizedNewtypeDeriving, EmptyCase, GADTs, AllowAmbiguousTypes, NoImplicitPrelude, TypeApplications, ScopedTypeVariables, NamedFieldPuns, TypeFamilies, RankNTypes #-}

module Step.Base where

-- The basics
import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..), maybe, isJust, isNothing, fromMaybe)
import Data.Functor (Functor (..), (<&>), ($>), (<$>), void)
import Data.Function (($), (&), (.), id, fix, on)
import Data.Either (Either (..), either)
import Control.Monad (Monad (..), (=<<))
import qualified Control.Monad as Monad
import Control.Applicative (Applicative (..))
import System.IO (IO)
import Data.Kind (Type)
import Data.Semigroup (Semigroup (..))
import Data.Monoid (Monoid (..))
import Prelude ((+), (-), error)
import Data.Functor.Contravariant (Predicate (..), contramap)
import Data.Eq (Eq ((==)))
import Data.Ord (Ord (compare))
import Text.Show (Show (showsPrec))

-- Containers
import Data.Sequence (Seq (..))
import qualified Data.ListLike as LL
import Data.ListLike (ListLike)

-- Optics
import Optics (view, preview, review, re, (%), iso, Iso, Iso')

-- Math
import Numeric.Natural (Natural)
import NatOptics.Positive.Unsafe (Positive (PositiveUnsafe))
import qualified NatOptics.Positive as Positive
import qualified NatOptics.Positive.Math as Positive
import qualified NatOptics.Signed as Signed
import Prelude (fromIntegral)

-- Effects
import Effectful
import Effectful.Dispatch.Static
import Effectful.Error.Static
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Effectful.Reader.Static
import Effectful.Internal.Env

-- Etc
import GHC.TypeLits (TypeError, ErrorMessage (Text))
import Data.Primitive.PrimArray
import GHC.Exts (IsList (..))

-- ⭕ The Chunk class

class Chunk xs x where
    leftView :: Iso' xs (Pop xs x)
    span :: Predicate x -> xs -> Span xs
    split :: Positive Natural -> xs -> Split xs
    drop :: Positive Natural -> xs -> Drop xs
    while :: Predicate x -> xs -> While xs
    length :: xs -> Positive Natural

data Pop xs x = Pop{ popItem :: x, popRemainder :: Maybe xs }

data Span xs =
    SpanAll
  | SpanNone
  | SpanPart{ spannedPart :: xs, spanRemainder :: xs }
  deriving stock (Eq, Ord, Show)

data Split xs = SplitInsufficient | Split xs xs
  deriving stock (Eq, Ord, Show)

data Drop xs =
    DropAll
  | DropInsufficient{ dropShortfall :: Positive Natural }
  | DropPart{ dropRemainder :: xs }
  deriving stock (Eq, Ord, Show)

data While xs = WhileNone | WhilePrefix xs | WhileAll

-- ⭕ ListLike chunks

data NonEmptyListLike xs x =
  NonEmptyListLike
    { nonEmptyListLike :: !xs
    , nonEmptyListLikeLength :: !(Positive Natural)
    }

assumeNonEmptyListLike :: ListLike xs (Item xs) => xs -> NonEmptyListLike xs x
assumeNonEmptyListLike xs = NonEmptyListLike xs (PositiveUnsafe (fromIntegral (LL.length xs)))

maybeNonEmptyListLike :: ListLike xs (Item xs) => xs -> Maybe (NonEmptyListLike xs x)
maybeNonEmptyListLike xs = preview Positive.natPrism (fromIntegral (LL.length xs)) <&> \l -> NonEmptyListLike xs l

instance Eq xs => Eq (NonEmptyListLike xs x) where
    (==) = (==) `on` nonEmptyListLike

instance Ord xs => Ord (NonEmptyListLike xs x) where
    compare = compare `on` nonEmptyListLike

instance Show xs => Show (NonEmptyListLike xs x) where
    showsPrec p = showsPrec p . nonEmptyListLike

instance ListLike xs x => Chunk (NonEmptyListLike xs x) x
  where
    length = nonEmptyListLikeLength

    span = \f whole ->
        tupleSpan (LL.span (getPredicate f) (nonEmptyListLike whole))
      where
        tupleSpan (a, b) =
            if LL.null b then SpanAll else
            if LL.null a then SpanNone else
            SpanPart (assumeNonEmptyListLike a) (assumeNonEmptyListLike b)

    drop = \n whole ->
        case Positive.minus (nonEmptyListLikeLength whole) n of
            Signed.Zero ->
                DropAll
            Signed.Plus _ ->
                DropPart
                  { dropRemainder = assumeNonEmptyListLike $
                      LL.drop (fromIntegral (review Positive.refine n)) (nonEmptyListLike whole)
                  }
            Signed.Minus dropShortfall ->
                DropInsufficient{ dropShortfall }

    while = \f x ->
        case maybeNonEmptyListLike
              (LL.takeWhile (getPredicate f) (nonEmptyListLike x))
          of
            Nothing -> WhileNone
            Just y ->
                if nonEmptyListLikeLength y == nonEmptyListLikeLength x
                then WhileAll
                else WhilePrefix y

    split = \n whole ->
        case Positive.minus (nonEmptyListLikeLength whole) n of
            Signed.Plus _ -> Split (assumeNonEmptyListLike a) (assumeNonEmptyListLike b)
              where
                (a, b) = LL.splitAt
                    (fromIntegral (review Positive.refine n))
                    (nonEmptyListLike whole)
            _ -> SplitInsufficient

    leftView = iso f g
      where
        f :: NonEmptyListLike xs x -> Pop (NonEmptyListLike xs x) x
        f a = a
            & nonEmptyListLike
            & LL.uncons
            & fromMaybe (error "ListLike leftViewIso")
            & \(x, b) -> Pop
                { popItem = x
                , popRemainder =
                    case Positive.minus (nonEmptyListLikeLength a) (PositiveUnsafe 1) of
                        Signed.Plus n -> Just (NonEmptyListLike b n )
                        _ -> Nothing
                }

        g :: Pop (NonEmptyListLike xs x) x -> NonEmptyListLike xs x
        g Pop{ popItem, popRemainder } = case popRemainder of
            Nothing -> NonEmptyListLike (LL.singleton popItem) (PositiveUnsafe 1)
            Just b -> NonEmptyListLike (LL.cons popItem (nonEmptyListLike b)) (Positive.plus (nonEmptyListLikeLength b) (PositiveUnsafe 1))

-- ⭕ The Buffer effect

data Buffer (bt :: BufferType) (c :: Type) :: Effect

data BufferType = CommitBuffer | ViewBuffer

type instance DispatchOf (Buffer bt c) = 'Static 'NoSideEffects

newtype instance StaticRep (Buffer bt c) = BufferSeq{ bufferSeq :: Seq c }
    deriving newtype (Semigroup, Monoid)

instance IsList (StaticRep (Buffer bt c)) where
    type Item (StaticRep (Buffer bt c)) = c
    fromList = BufferSeq . fromList
    toList = toList . bufferSeq

runBuffer :: forall bt c es a. Eff (Buffer bt c ': es) a -> Eff es a
runBuffer = runBuffer' mempty

runBuffer' :: forall bt c es a. Seq c -> Eff (Buffer bt c ': es) a -> Eff es a
runBuffer' = evalStaticRep . BufferSeq

getBufferSeq :: forall bt c es. Buffer bt c :> es => Eff es (Seq c)
getBufferSeq = getStaticRep @(Buffer bt c) <&> bufferSeq

putBufferSeq :: forall bt c es. Buffer bt c :> es => Seq c -> Eff es ()
putBufferSeq = putStaticRep @(Buffer bt c) . BufferSeq

feedBuffer :: forall bt c es. Buffer bt c :> es => c -> Eff es ()
feedBuffer x = getBufferSeq @bt @c >>= \xs -> putBufferSeq @bt @c (xs :|> x)

returnToBuffer :: forall bt c es. Buffer bt c :> es => c -> Eff es ()
returnToBuffer x = getBufferSeq @bt >>= \xs -> putBufferSeq @bt (x :<| xs)

takeBufferChunk :: forall bt c es. Buffer bt c :> es => Eff es (Maybe c)
takeBufferChunk = getBufferSeq @bt >>= \case
    Empty -> return Nothing
    y :<| ys -> putBufferSeq @bt ys $> Just y

dropFromBuffer :: forall bt c x es. Chunk c x => Buffer bt c :> es => Positive Natural -> Eff es AdvanceResult
dropFromBuffer = fix \r n -> getBufferSeq @bt @c >>= \case
    Empty -> return YouCanNotAdvance{ shortfall = n }
    x :<| xs -> case drop @c @x n x of
        DropAll -> putBufferSeq @bt xs $> AdvanceSuccess
        DropPart{ dropRemainder } -> putBufferSeq @bt (dropRemainder :<| xs) $> AdvanceSuccess
        DropInsufficient{ dropShortfall } -> putBufferSeq @bt xs *> r dropShortfall

data AdvanceResult =
    AdvanceSuccess
  | YouCanNotAdvance{ shortfall :: Positive Natural }

-- ⭕ The Load effect

data Load (c :: Type) :: Effect
  where
    Load :: Load c m (Maybe c)

type instance DispatchOf (Load c) = 'Dynamic

load :: Load c :> es => Eff es (Maybe c)
load = send Load

loadFromList :: [c] -> Eff (Load c ': es) a-> Eff es (a, [c])
loadFromList xs = reinterpret (runState xs) loadFromListHandler

loadFromListHandler :: State [c] :> es => EffectHandler (Load c) (es)
loadFromListHandler _env Load = get >>= \case
    [] -> return Nothing
    x : xs -> put xs $> Just x

-- ⭕ The Step effect

data Step (mo :: Mode) (p :: Perfection) (c :: Type) (x :: Type) (e :: Type) :: Effect
  where
    StepCommit :: Positive Natural -> Step 'ReadWrite p c x m e AdvanceResult
    StepNext :: Step mo p c x m e (Maybe c)
    StepReset :: Step mo p c x m e ()
    StepFail :: e -> Step mo 'Imperfect c x e m a

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

runStep :: forall c x e es a. Chunk c x => '[Load c, Buffer 'CommitBuffer c, Error e] :>> es =>
    Eff (Step 'ReadWrite 'Imperfect c x e ': es) a -> Eff es a
runStep = reinterpret runStepHandler stepHandler

runStepHandler :: forall c es a. Buffer 'CommitBuffer c :> es => Eff (Buffer 'ViewBuffer c ': es) a -> Eff es a
runStepHandler a = getBufferSeq @'CommitBuffer @c >>= \b -> runBuffer' @'ViewBuffer @c b a

stepHandler :: forall c x es e. Chunk c x => '[Load c, Buffer 'CommitBuffer c, Error e] :>> es =>
    EffectHandler (Step 'ReadWrite 'Imperfect c x e) (Buffer 'ViewBuffer c ': es)
stepHandler _env = \case
    StepFail e -> throwError e
    StepReset -> getBufferSeq @'CommitBuffer @c >>= putBufferSeq @'ViewBuffer @c
    StepNext -> runStepNext @c
    StepCommit n -> runStepCommit @c @x n

runStepNext :: forall c es. '[Load c, Buffer 'ViewBuffer c, Buffer 'CommitBuffer c] :>> es => Eff es (Maybe c)
runStepNext = takeBufferChunk @'ViewBuffer >>= \case
    Just x -> return (Just x)
    Nothing -> bufferMore @c >>= \case{ True -> takeBufferChunk @'ViewBuffer; False -> return Nothing }

runStepCommit :: forall c x es. Chunk c x => '[Load c, Buffer 'ViewBuffer c, Buffer 'CommitBuffer c] :>> es => Positive Natural -> Eff es AdvanceResult
runStepCommit n = dropFromBuffer @'CommitBuffer @c @x n >>= \case
    r@AdvanceSuccess -> return r
    r@YouCanNotAdvance{ shortfall = n' } -> bufferMore @c >>= \case{ True -> dropFromBuffer @'CommitBuffer @c @x n'; False -> return r }

bufferMore :: forall c es. '[Load c, Buffer 'ViewBuffer c, Buffer 'CommitBuffer c] :>> es => Eff es Bool
bufferMore = load @c >>= \case
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

takeCharMaybe :: Chunk c x => Reader e :> es => Sure c x es e (Maybe x)
takeCharMaybe = try takeChar

takeChar :: Chunk c x => Reader e :> es => AtomicMove c x es e x
takeChar = nextChar `bindAction` \x -> commit one $> x

nextChar :: Chunk c x => Reader e :> es => Query c x es e x
nextChar = nextCharMaybe `bindAction` maybe (castTo @Query fail) return

nextMaybe :: SureQuery c x es e (Maybe c)
nextMaybe = reset `bindAction` \() -> nextMaybe'

-- | Like 'nextMaybe', but doesn't reset first
nextMaybe' :: forall c x es e. SureQuery c x es e (Maybe c)
nextMaybe' = SureQuery $ send @(Step 'ReadOnly 'Perfect c x e) StepNext

next :: Reader e :> es => Query c x es e c
next = nextMaybe `bindAction` maybe (castTo @Query fail) return

-- | Like 'next', but doesn't reset first
next' :: Reader e :> es => Query c x es e c
next' = nextMaybe' `bindAction` maybe (castTo @Query fail) return

takeNext :: forall c x e es. Chunk c x => Reader e :> es => AtomicMove c x es e c
takeNext = next `bindAction` \xs -> commit (length @c @x xs) $> xs

takeNextMaybe :: Chunk c x => Reader e :> es => Sure c x es e (Maybe c)
takeNextMaybe = try takeNext

nextCharMaybe :: Chunk c x => SureQuery c x es e (Maybe x)
nextCharMaybe = nextMaybe <&> fmap @Maybe (popItem . view leftView)

satisfyJust :: Chunk c x => Reader e :> es => (x -> Maybe a) -> AtomicMove c x es e a
satisfyJust ok = nextCharMaybe `bindAction` \x -> case x >>= ok of Nothing -> castTo fail; Just y -> commit one $> y

skip0 :: forall c x e es. Chunk c x => Reader e :> es => Natural -> Any c x es e ()
skip0 = maybe (return ()) (castTo @Any . skip)  . preview Positive.refine

skip :: forall c x e es. Chunk c x => Reader e :> es => Positive Natural -> Move c x es e ()
skip n = next `bindAction` \x ->
    case Positive.minus (length @c @x x) n of
        Signed.Minus n' ->
            commit (length @c @x x) `bindAction` \_ -> skip n'
        _ -> castTo @Move (commit n)

skipAtomically0 :: forall c x e es. Chunk c x => Reader e :> es => Natural -> Atom c x es e ()
skipAtomically0 = maybe (trivial ()) (castTo @Atom . skipAtomically)  . preview Positive.refine

skipAtomically :: forall c x e es. Chunk c x => Reader e :> es => Positive Natural -> AtomicMove c x es e ()
skipAtomically n = ensureAtLeast n `bindAction` \() -> commit n

ensureAtLeast :: forall c x e es. Chunk c x => Reader e :> es => Positive Natural -> Query c x es e ()
ensureAtLeast = \n -> castTo @Query reset `bindAction` \() -> go n
  where
    go :: Reader e :> es => Positive Natural -> Query c x es e ()
    go n = next' `bindAction` \x ->
        case Positive.minus n (length @c @x x) of
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
