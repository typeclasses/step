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
import Data.Foldable (traverse_)
import Data.Traversable (traverse)
import Data.Void (Void, absurd)

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

-- Transformers
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Control.Monad.Reader as MTL
import qualified Control.Monad.Trans.Maybe as MTL

-- Streaming
import qualified List.Transformer as ListT
import List.Transformer (ListT)

-- Etc
import GHC.TypeLits (TypeError, ErrorMessage (Text))
import GHC.Exts (IsList (..))

-- ⭕ Client

newtype Client req a = Client (forall m. Monad m => (forall b. req b -> m b) -> m a)
    deriving stock Functor

instance Applicative (Client req) where
    pure = return
    (<*>) = Monad.ap

instance Monad (Client req) where
    return x = Client (\_ -> return x)
    Client a >>= f = Client \send -> a send >>= \x -> let Client b = f x in b send

-- Client transformations

mapRequest :: (forall b. req1 b -> req2 b) -> Client req1 a -> Client req2 a
mapRequest f (Client a) = Client \send -> a (send . f)

mapMaybeRequest :: (forall b. req1 b -> Maybe (req2 b)) -> Client req1 a -> Client req2 (Maybe a)
mapMaybeRequest f (Client a) = Client \send -> MTL.runMaybeT $ a \r -> MaybeT $ traverse send $ f r

-- ⭕ OneOf

type family OneOf (c :: Type) :: Type

-- ⭕ Step

data Step (mo :: Mode) (c :: Type) (m :: Type -> Type) (e :: Type) (a :: Type)
  where
    StepCommit :: Positive Natural -> Step 'ReadWrite c m e AdvanceResult
    StepNext :: Step mo c m e (Maybe c)
    StepReset :: Step mo c m e ()
    StepFail :: e -> Step mo c m e a
    StepLift :: m a -> Step mo c m e a

data Mode = ReadOnly | ReadWrite

data AdvanceResult =
    AdvanceSuccess
  | YouCanNotAdvance{ shortfall :: Positive Natural }

-- Step conversions

castStepMode :: Step 'ReadOnly c m e a -> Step 'ReadWrite c m e a
castStepMode = \case
    StepNext -> StepNext
    StepReset -> StepReset
    StepFail x -> StepFail x
    StepLift x -> StepLift x

castStepPerfection :: Step mo c m Void a -> Step mo c m e a
castStepPerfection = \case
    StepNext -> StepNext
    StepReset -> StepReset
    StepCommit x -> StepCommit x
    StepLift x -> StepLift x
    StepFail x -> absurd x

castStepDual :: Step 'ReadOnly c m Void a -> Step 'ReadWrite c m e a
castStepDual = \case
    StepNext -> StepNext
    StepReset -> StepReset
    StepLift x -> StepLift x
    StepFail x -> absurd x

tryStep :: Step mo c m e a -> Maybe (Step mo c m e' a)
tryStep = \case
    StepFail _ -> Nothing
    StepNext -> Just StepNext
    StepReset -> Just StepReset
    StepCommit n -> Just (StepCommit n)
    StepLift x -> Just (StepLift x)

--

failStepClientM :: m e -> Client (Step mo c m e) a
failStepClientM x = Client \send -> send (StepLift x) >>= \e -> send (StepFail e)

-- ⭕ Action

type Action = Type -> (Type -> Type) -> Type -> Type -> Type

-- Simple actions that are just a newtype for an action with a Step effect

type Any :: Action
type Query :: Action
type Sure :: Action
type SureQuery :: Action

-- | The most general of the actions
newtype Any c m e a = Any (Client (Step 'ReadWrite c m e) a)
    deriving newtype (Functor, Applicative, Monad)

-- | Like 'Any', but cannot move the cursor
newtype Query c m e a = Query (Client (Step 'ReadOnly c m e) a)
    deriving newtype (Functor, Applicative, Monad)

-- | Always succeeds
newtype Sure c m e a = Sure (Client (Step 'ReadWrite c m Void) a)
    deriving newtype (Functor, Applicative, Monad)

-- | Always succeeds, does not move the cursor
newtype SureQuery c m e a = SureQuery (Client (Step 'ReadOnly c m Void) a)
    deriving newtype (Functor, Applicative, Monad)

-- Actions defined in terms of others

type Atom :: Action
type Move :: Action
type AtomicMove :: Action

-- | Fails noncommittally; see 'try'
newtype Atom c m e a = Atom (Query c m e (Sure c m e a))
    deriving stock (Functor)

-- | Always moves the cursor
newtype Move c m e a = Move (Any c m e a)
    deriving stock (Functor)

-- | Always moves the cursor, is atomic
newtype AtomicMove c m e a = AtomicMove (Atom c m e a)
    deriving stock (Functor)

instance (TypeError ('Text "Atom cannot be Applicative because (<*>) would not preserve atomicity")) => Applicative (Atom c m e) where
    pure = error "unreachable"
    (<*>) = error "unreachable"

instance (TypeError ('Text "Move cannot be Applicative because 'pure' would not move the cursor")) =>
    Applicative (Move c m e)
  where
    pure = error "unreachable"
    (<*>) = error "unreachable"

instance (TypeError ('Text "AtomicMove cannot be Applicative because 'pure' would not move the cursor and (<*>) would not preserve atomicity")) => Applicative (AtomicMove c e m) where
    pure = error "unreachable"
    (<*>) = error "unreachable"

-- Failure action

newtype Failure c m e a = Failure (m e)
    deriving stock (Functor)

instance (TypeError ('Text "Failure cannot be Applicative because 'pure' would succeed")) => Applicative (Failure c m e) where
    pure = error "unreachable"
    (<*>) = error "unreachable"

-- ⭕ Miscellaneous classes of action

-- | Action that can return a value and do nothing else
class Trivial (act :: Action) where
    trivial :: a -> act c e m a

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

-- | Action that can fail
class Fallible (act :: Action) where
    failActionM :: m e -> act c m e a

instance Fallible Any where
    failActionM = Any . failStepClientM

instance Fallible Query where
    failActionM = Query . failStepClientM

instance Fallible Move where
    failActionM = Move . Any . failStepClientM

instance Fallible Atom where
    failActionM = Atom . Query . failStepClientM

instance Fallible AtomicMove where
    failActionM = AtomicMove . Atom . Query . failStepClientM

-- | Action that can be tried noncommittally
class Atomic (act :: Action) (try :: Action) | act -> try where
    try :: act c m e a -> try c m e (Maybe a)

instance Atomic Atom Sure where
    try (Atom (Query q)) =
        Sure (mapMaybeRequest (tryStep . castStepMode) q)
        >>= maybe (return Nothing) (fmap Just)

instance Atomic AtomicMove Sure where
    try (AtomicMove a) = try a

instance Atomic Query SureQuery where
    try (Query q) = SureQuery (mapMaybeRequest tryStep q)

-- | Unsafe coercion to action that always moves
class Is act2 act1 => AssumeMovement (act1 :: Action) (act2 :: Action) | act1 -> act2 where
    assumeMovement :: act1 c es e a -> act2 c es e a

instance AssumeMovement Any Move where
    assumeMovement = Move

instance AssumeMovement Atom AtomicMove where
    assumeMovement = AtomicMove

-- ⭕ Action subtype relationship

class Is (act1 :: Action) (act2 :: Action) where
    cast :: act1 c es e a -> act2 c es e a

-- | Same as 'cast', but with type parameters reordered so that the action we're casting to is first, which is more convenient for type application in some circumstances
castTo :: forall act2 act1 c es e a. Is act1 act2 => act1 c es e a -> act2 c es e a
castTo = cast @act1 @act2

cast2 :: forall act2 act1 f c es e a. Is act1 act2 => Functor f => f (act1 c es e a) -> f (act2 c es e a)
cast2 = fmap (castTo @act2)

-- Everything is itself

instance {-# overlappable #-} Is a a where
    cast = id

-- Casting actions via casting steps

instance Is SureQuery Sure where
    cast (SureQuery x) = Sure (mapRequest castStepMode x)

instance Is Query Any where
    cast (Query x) = Any (mapRequest castStepMode x)

instance Is SureQuery Query where
    cast (SureQuery x) = Query (mapRequest castStepPerfection x)

instance Is Sure Any where
    cast (Sure x) = Any (mapRequest castStepPerfection x)

instance Is SureQuery Any where
    cast (SureQuery x) = Any (mapRequest castStepDual x)

-- Casting to Atom

instance Is SureQuery Atom where
    cast = Atom . fmap return . castTo @Query

instance Is Query Atom where
    cast = Atom . fmap return

instance Is Sure Atom where
    cast = Atom . return

-- Casting out of atomicity

instance Is Atom Any where
    cast (Atom x) = Monad.join (cast @Sure @Any <$> cast @Query @Any x)

instance Is AtomicMove Move where
    cast (AtomicMove x) = Move (cast @Atom @Any x)

-- Casting out of movement

instance Is Move Any where
    cast (Move x) = x

instance Is AtomicMove Atom where
    cast (AtomicMove x) = x

-- Casting out of both atomicity and movement

instance Is AtomicMove Any where
    cast = cast . castTo @Move

-- Casting out of failure

instance Fallible a => Is Failure a where
    cast (Failure x) = failActionM x

-- ⭕ Join

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

class Join (act1 :: Action) (act2 :: Action) where
    join :: act1 c es e (act2 c es e a) -> (act1 >> act2) c es e a

infixl 1 `bindAction`
bindAction :: (Join act1 act2, Functor (act1 c es e), act1 >> act2 ~ act3) =>
    act1 c es e a -> (a -> act2 c es e b) -> act3 c es e b
bindAction x f = join (fmap f x)

-- Any >> ...

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

-- Atom >> ...

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

-- AtomicMove >> ...

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

-- Failure >> ...

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

-- Move >> ...

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

-- Query >> ...

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

-- Sure >> ...

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

 -- SureQuery >> ...

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

-- ⭕ Chunk

class Chunk c where
    leftView :: Iso' c (Pop c)
    span :: Predicate (OneOf c) -> c -> Span c
    split :: Positive Natural -> c -> Split c
    drop :: Positive Natural -> c -> Drop c
    while :: Predicate (OneOf c) -> c -> While c
    length :: c -> Positive Natural

data Pop c = Pop{ popItem :: OneOf c, popRemainder :: Maybe c }

data Span c =
    SpanAll
  | SpanNone
  | SpanPart{ spannedPart :: c, spanRemainder :: c }
  deriving stock (Eq, Ord, Show)

data Split c = SplitInsufficient | Split c c
  deriving stock (Eq, Ord, Show)

data Drop c =
    DropAll
  | DropInsufficient{ dropShortfall :: Positive Natural }
  | DropPart{ dropRemainder :: c }
  deriving stock (Eq, Ord, Show)

data While c = WhileNone | WhilePrefix c | WhileAll

-- ListLike chunks

data NonEmptyListLike c =
  NonEmptyListLike
    { nonEmptyListLike :: !c
    , nonEmptyListLikeLength :: !(Positive Natural)
    }

assumeNonEmptyListLike :: ListLike c (Item c) => c -> NonEmptyListLike c
assumeNonEmptyListLike c = NonEmptyListLike c (PositiveUnsafe (fromIntegral (LL.length c)))

maybeNonEmptyListLike :: ListLike c (Item c) => c -> Maybe (NonEmptyListLike c)
maybeNonEmptyListLike c = preview Positive.natPrism (fromIntegral (LL.length c)) <&> \l -> NonEmptyListLike c l

instance Eq c => Eq (NonEmptyListLike c) where
    (==) = (==) `on` nonEmptyListLike

instance Ord c => Ord (NonEmptyListLike c) where
    compare = compare `on` nonEmptyListLike

instance Show c => Show (NonEmptyListLike c) where
    showsPrec p = showsPrec p . nonEmptyListLike

type instance OneOf (NonEmptyListLike c) = Item c

instance ListLike c (Item c) => Chunk (NonEmptyListLike c)
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
        f :: NonEmptyListLike c -> Pop (NonEmptyListLike c)
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

        g :: Pop (NonEmptyListLike c) -> NonEmptyListLike c
        g Pop{ popItem, popRemainder } = case popRemainder of
            Nothing -> NonEmptyListLike (LL.singleton popItem) (PositiveUnsafe 1)
            Just b -> NonEmptyListLike (LL.cons popItem (nonEmptyListLike b)) (Positive.plus (nonEmptyListLikeLength b) (PositiveUnsafe 1))

-- ⭕ Prelude of actions

commit :: forall c es e. Positive Natural -> AtomicMove c es e ()
commit n = AtomicMove $ Atom $ Query $ return $ Sure $ void $ Client \send -> send $ StepCommit n

fail :: MonadReader e m => Failure c m e a
fail = Failure MTL.ask

takeCharMaybe :: Chunk c => MonadReader e m => Sure c m e (Maybe (OneOf c))
takeCharMaybe = try takeChar

takeChar :: Chunk c => MonadReader e m => AtomicMove c m e (OneOf c)
takeChar = nextChar `bindAction` \x -> commit one $> x

nextChar :: Chunk c => MonadReader e m => Query c m e (OneOf c)
nextChar = nextCharMaybe `bindAction` maybe (castTo @Query fail) return

nextMaybe :: SureQuery c m e (Maybe c)
nextMaybe = reset `bindAction` \() -> nextMaybe'

-- | Like 'nextMaybe', but doesn't reset first
nextMaybe' :: forall c m e. SureQuery c m e (Maybe c)
nextMaybe' = SureQuery $ Client \send -> send StepNext

next :: MonadReader e m => Query c m e c
next = nextMaybe `bindAction` maybe (castTo @Query fail) return

-- | Like 'next', but doesn't reset first
next' :: MonadReader e m => Query c m e c
next' = nextMaybe' `bindAction` maybe (castTo @Query fail) return

takeNext :: forall c e m. Chunk c => MonadReader e m => AtomicMove c m e c
takeNext = next `bindAction` \c -> commit (length @c c) $> c

takeNextMaybe :: Chunk c => MonadReader e m => Sure c m e (Maybe c)
takeNextMaybe = try takeNext

nextCharMaybe :: Chunk c => SureQuery c m e (Maybe (OneOf c))
nextCharMaybe = nextMaybe <&> fmap @Maybe (popItem . view leftView)

satisfyJust :: Chunk c => MonadReader e m => (OneOf c -> Maybe a) -> AtomicMove c m e a
satisfyJust ok = nextCharMaybe `bindAction` \x -> case x >>= ok of Nothing -> castTo fail; Just y -> commit one $> y

skip0 :: forall c e m. Chunk c => MonadReader e m => Natural -> Any c m e ()
skip0 = maybe (return ()) (castTo @Any . skip)  . preview Positive.refine

skip :: forall c e m. Chunk c => MonadReader e m => Positive Natural -> Move c m e ()
skip n = next `bindAction` \x ->
    case Positive.minus (length @c x) n of
        Signed.Minus n' ->
            commit (length @c x) `bindAction` \_ -> skip n'
        _ -> castTo @Move (commit n)

skipAtomically0 :: forall c e m. Chunk c => MonadReader e m => Natural -> Atom c m e ()
skipAtomically0 = maybe (trivial ()) (castTo @Atom . skipAtomically)  . preview Positive.refine

skipAtomically :: forall c e m. Chunk c => MonadReader e m => Positive Natural -> AtomicMove c m e ()
skipAtomically n = ensureAtLeast n `bindAction` \() -> commit n

ensureAtLeast :: forall c e m. Chunk c => MonadReader e m => Positive Natural -> Query c m e ()
ensureAtLeast = \n -> castTo @Query reset `bindAction` \() -> go n
  where
    go :: MonadReader e m => Positive Natural -> Query c m e ()
    go n = next' `bindAction` \x ->
        case Positive.minus n (length @c x) of
            Signed.Plus n' -> go n'
            _ -> return ()

atEnd :: SureQuery c es e Bool
atEnd = reset `bindAction` \() -> nextMaybe' <&> isNothing

end :: MonadReader e m => Query c m e ()
end = atEnd `bindAction` \e -> if e then trivial () else castTo @Query fail

reset :: forall c es e. SureQuery c es e ()
reset = SureQuery $ Client \send -> send StepReset

-- ⭕

one :: Positive Natural
one = PositiveUnsafe 1

-- ⭕ The Buffer effect

-- data Buffer (bt :: BufferType) (c :: Type) :: Effect

-- data BufferType = CommitBuffer | ViewBuffer

-- type instance DispatchOf (Buffer bt c) = 'Static 'NoSideEffects

-- newtype instance StaticRep (Buffer bt c) = BufferSeq{ bufferSeq :: Seq c }
--     deriving newtype (Semigroup, Monoid)

-- instance IsList (StaticRep (Buffer bt c)) where
--     type Item (StaticRep (Buffer bt c)) = c
--     fromList = BufferSeq . fromList
--     toList = toList . bufferSeq

-- runBuffer :: forall bt c es a. Eff (Buffer bt c ': es) a -> Eff es a
-- runBuffer = runBuffer' mempty

-- runBuffer' :: forall bt c es a. Seq c -> Eff (Buffer bt c ': es) a -> Eff es a
-- runBuffer' = evalStaticRep . BufferSeq

-- getBufferSeq :: forall bt c es. Buffer bt c :> es => Eff es (Seq c)
-- getBufferSeq = getStaticRep @(Buffer bt c) <&> bufferSeq

-- putBufferSeq :: forall bt c es. Buffer bt c :> es => Seq c -> Eff es ()
-- putBufferSeq = putStaticRep @(Buffer bt c) . BufferSeq

-- feedBuffer :: forall bt c es. Buffer bt c :> es => c -> Eff es ()
-- feedBuffer x = getBufferSeq @bt @c >>= \c -> putBufferSeq @bt @c (c :|> x)

-- returnToBuffer :: forall bt c es. Buffer bt c :> es => c -> Eff es ()
-- returnToBuffer x = getBufferSeq @bt >>= \c -> putBufferSeq @bt (x :<| c)

-- takeBufferChunk :: forall bt c es. Buffer bt c :> es => Eff es (Maybe c)
-- takeBufferChunk = getBufferSeq @bt >>= \case
--     Empty -> return Nothing
--     y :<| ys -> putBufferSeq @bt ys $> Just y

-- dropFromBuffer :: forall bt c x es. Chunk c x => Buffer bt c :> es => Positive Natural -> Eff es AdvanceResult
-- dropFromBuffer = fix \r n -> getBufferSeq @bt @c >>= \case
--     Empty -> return YouCanNotAdvance{ shortfall = n }
--     x :<| c -> case drop @c @x n x of
--         DropAll -> putBufferSeq @bt c $> AdvanceSuccess
--         DropPart{ dropRemainder } -> putBufferSeq @bt (dropRemainder :<| c) $> AdvanceSuccess
--         DropInsufficient{ dropShortfall } -> putBufferSeq @bt c *> r dropShortfall

-- ⭕

-- stepHandler :: forall c x es e. Chunk c x => '[Load c, Buffer 'CommitBuffer c, Error e] :>> es =>
--     EffectHandler (Step 'ReadWrite 'Imperfect c x e) (Buffer 'ViewBuffer c ': es)
-- stepHandler _env = \case
--     StepFail e -> throwError e
--     StepReset -> getBufferSeq @'CommitBuffer @c >>= putBufferSeq @'ViewBuffer @c
--     StepNext -> runStepNext @c
--     StepCommit n -> runStepCommit @c @x n

-- runStepNext :: forall c es. '[Load c, Buffer 'ViewBuffer c, Buffer 'CommitBuffer c] :>> es => Eff es (Maybe c)
-- runStepNext = takeBufferChunk @'ViewBuffer >>= \case
--     Just x -> return (Just x)
--     Nothing -> bufferMore @c >>= \case{ True -> takeBufferChunk @'ViewBuffer; False -> return Nothing }

-- runStepCommit :: forall c x es. Chunk c x => '[Load c, Buffer 'ViewBuffer c, Buffer 'CommitBuffer c] :>> es => Positive Natural -> Eff es AdvanceResult
-- runStepCommit n = dropFromBuffer @'CommitBuffer @c @x n >>= \case
--     r@AdvanceSuccess -> return r
--     r@YouCanNotAdvance{ shortfall = n' } -> bufferMore @c >>= \case{ True -> dropFromBuffer @'CommitBuffer @c @x n'; False -> return r }

-- bufferMore :: forall c es. '[Load c, Buffer 'ViewBuffer c, Buffer 'CommitBuffer c] :>> es => Eff es Bool
-- bufferMore = load @c >>= \case
--     Nothing -> return False
--     Just x -> do{ feedBuffer @'CommitBuffer x; feedBuffer @'ViewBuffer x; return True }

-- parseOnlyListT :: forall k c x es e a. Chunk c x => Is k Any => k c x es e a -> ListT (Eff es) c -> Eff es (Either e a)
-- parseOnlyListT a c = let Any eff = cast a in
--     _
--     $ runError
--     $ runStep @c @x @e
--     $ swapEff4123
--     $ addEffect @(Error e)
--     $ addEffect @(Load c)
--     $ addEffect @(Buffer 'CommitBuffer c)
--     $ eff
