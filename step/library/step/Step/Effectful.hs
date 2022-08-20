{-# language UndecidableInstances #-}
{-# language DataKinds, KindSignatures, InstanceSigs, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# language ConstraintKinds, DataKinds, FlexibleContexts, KindSignatures, QuantifiedConstraints, TypeOperators #-}
{-# language DataKinds, StandaloneKindSignatures, FunctionalDependencies, FlexibleInstances #-}
{-# language DeriveAnyClass, DeriveFunctor, DerivingVia, GeneralizedNewtypeDeriving, EmptyCase, GADTs #-}

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

-- ⭕ The Buffer data structure

newtype Buffer xs x = Buffer{ bufferToSeq :: Seq (Nontrivial xs x) }
    deriving newtype (Semigroup, Monoid)

instance IsList (Buffer xs x) where
    type Item (Buffer xs x) = Nontrivial xs x
    fromList = Buffer . fromList
    toList = toList . bufferToSeq

feedBuffer :: State (Buffer xs x) :> es => Nontrivial xs x -> Eff es ()
feedBuffer x = gets bufferToSeq >>= \xs -> put (Buffer (xs :|> x))

returnToBuffer :: State (Buffer xs x) :> es => Nontrivial xs x -> Eff es ()
returnToBuffer x = gets bufferToSeq >>= \xs -> put (Buffer (x :<| xs))

takeBufferChunk :: State (Buffer xs x) :> es => Eff es (Maybe (Nontrivial xs x))
takeBufferChunk = gets bufferToSeq >>= \case
    Empty -> return Nothing
    y :<| ys -> put (Buffer ys) $> Just y

dropFromBuffer :: State (Buffer xs x) :> es =>
    NT.DropOperation xs x
    -> Positive Natural
    -> Eff es AdvanceResult
dropFromBuffer NT.DropOperation{ NT.drop } = fix \r n -> gets bufferToSeq >>= \case
    Empty -> return YouCanNotAdvance{ shortfall = n }
    x :<| xs -> case drop n x of
        NT.DropAll -> put (Buffer xs) $> AdvanceSuccess
        NT.DropPart{ NT.dropRemainder } -> put (Buffer (dropRemainder :<| xs)) $> AdvanceSuccess
        NT.DropInsufficient{ NT.dropShortfall } -> put (Buffer xs) *> r dropShortfall

-- ⭕ Phantoms

data Perfection = Perfect | Imperfect

data Mode = ReadOnly | ReadWrite

-- ⭕ The Load effect

data Load (xs :: Type) (x :: Type) :: Effect
  where
    Load :: Load xs x m (Maybe (Nontrivial xs x))

type instance DispatchOf (Load xs x) = 'Dynamic

-- ⭕ The CommitBufferState effect

data CommitBufferState (xs :: Type) (x :: Type) :: Effect

type instance DispatchOf (CommitBufferState xs x) = 'Static 'NoSideEffects

newtype instance StaticRep (CommitBufferState xs x) = CommitBuffer{ unCommitBuffer :: Buffer xs x }

getCommitBuffer :: forall xs x es. CommitBufferState xs x :> es => Eff es (Buffer xs x)
getCommitBuffer = getStaticRep <&> unCommitBuffer

-- ⭕ The ViewBufferState effect

data ViewBufferState (xs :: Type) (x :: Type) :: Effect

type instance DispatchOf (ViewBufferState xs x) = 'Static 'NoSideEffects

newtype instance StaticRep (ViewBufferState xs x) = ViewBuffer (Buffer xs x)

evalViewBufferState :: Buffer xs x -> Eff (ViewBufferState xs x ': es) a -> Eff es a
evalViewBufferState = evalStaticRep . ViewBuffer

putViewBuffer :: ViewBufferState xs x :> es => Buffer xs x -> Eff es ()
putViewBuffer = putStaticRep . ViewBuffer

-- ⭕ The Step effect

data Step (mo :: Mode) (p :: Perfection) (xs :: Type) (x :: Type) (e :: Type) :: Effect
  where
    Commit :: Positive Natural -> Step 'ReadWrite p xs x m e AdvanceResult
    Next :: Step mo p xs x m e (Maybe (Nontrivial xs x))
    Reset :: Step mo p xs x m e ()
    Fail :: e -> Step mo 'Imperfect xs x e m ()

type instance DispatchOf (Step mo p xs x e) = 'Dynamic

runStep :: forall xs x e es a. '[Load xs x, CommitBufferState xs x, Error e] :>> es => NT.DropOperation xs x
    -> Eff (Step 'ReadWrite 'Imperfect xs x e ': es) a -> Eff es a
runStep dropOp = reinterpret @(Step 'ReadWrite 'Imperfect xs x e)
    (\a -> getCommitBuffer @xs @x >>= \b -> evalViewBufferState @xs @x b a)
    (\_env act ->
          case act of
              Fail e -> throwError e
              Reset -> getCommitBuffer @xs @x >>= putViewBuffer @xs @x
              -- Next ->
    )

-- g :: forall xs x es e. '[Load xs x, CommitBufferState xs x, Error e, ViewBufferState xs x] :>> es =>
--     NT.DropOperation xs x
--     -> EffectHandler (Step 'ReadWrite 'Imperfect xs x e) es
--     -- ViewBufferState xs x :> localEs =>
--     -- -> LocalEnv localEs es
--     -- -> Step 'ReadWrite 'Imperfect xs x e (Eff localEs) a
--     -- -> Eff es a

{-
    \(env :: LocalEnv localEs (State (Buffer xs x) ': es)) ->
        let
          bufferMore :: SharedSuffix es (State (Buffer xs x) ': es) => Eff (State (Buffer xs x) ': es) ()
          bufferMore = _
              -- localSeqUnlift env \unlift ->
              -- localSeqUnlift env \unlift -> unlift (send Load) >>= \case
              --     Nothing -> return ()
              --     Just x -> do
              --         getStaticRep @(CommitBufferState xs x) >>= \(CommitBuffer b) ->
              --             putStaticRep (CommitBuffer (runPureEff $ execState b (feedBuffer x)))
              --         unlift (get >>= \(ViewBuffer b) ->
              --             put (runState b (feedBuffer x)))

          commitBuffered :: Positive Natural -> Eff (State (Buffer xs x) : es) AdvanceResult
          commitBuffered n = getStaticRep @(CommitBufferState xs x) >>= \(CommitBuffer b) ->
              runState b (dropFromBuffer dropOp n) >>= \(ar, b') -> putStaticRep (CommitBuffer b') $> ar
        in
        \case
          Fail e -> throwError e
          Reset -> getStaticRep @(CommitBufferState xs x) >>= \(CommitBuffer b) -> put b
          -- Next -> localSeqUnlift env \unlift -> unlift $ get >>= \(ViewBuffer b) ->
          --     runState b takeBufferChunk >>= \(xm, b') -> putStaticRep (CommitBufferState b') $> xm
          Commit n -> commitBuffered n >>= \case
              r@AdvanceSuccess -> return r
              YouCanNotAdvance n' -> bufferMore *> commitBuffered n'
-}

-- ⭕ Simple actions that are just a newtype for an action with a Step effect

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

-- | Fails noncommittally; see 'try'
newtype Atom xs x es e a = Atom{ unAtom :: Query xs x es e (Sure xs x es e a) }
    deriving stock (Functor)

-- | Always moves the cursor
newtype Move xs x es e a = Move (Any xs x es e a)
    deriving stock (Functor)

-- | Always moves the cursor, is atomic
newtype AtomicMove xs x es e a = AtomicMove{ unAtomicMove :: Atom xs x es e a }
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

-- ⭕

data AdvanceResult =
    AdvanceSuccess
  | YouCanNotAdvance{ shortfall :: Positive Natural }

-- ⭕

-- pureFeed :: State [Nontrivial xs x] :> es => Feed xs x es
-- pureFeed =
--   Feed
--     { upstream = get >>= \case [] -> return Nothing; x : xs -> put xs $> Just x
--     }

-- ⭕

-- runAny :: forall xs x e es extra a.
--     Input xs x :> es =>
--     Subset extra es =>
--     Any xs x extra e a -> Eff es a
-- runAny (Any (act :: Eff (Step 'ReadWrite 'Imperfect xs x : extra) a)) =
--     inject $ reinterpret @(Step 'ReadWrite 'Imperfect xs x) @'[State (Buffer xs x)] f g act
--   where
--     f :: Eff (State (Buffer xs x) : es) a -> Eff es a
--     f = evalState (Buffer mempty)

--     g :: LocalEnv localEs '[State (Buffer xs x)] -> Step 'ReadWrite 'Imperfect xs x (Eff localEs) a1 -> Eff '[State (Buffer xs x)] a1
--     g = _
