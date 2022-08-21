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

-- ⭕ The Load effect

data Load (xs :: Type) (x :: Type) :: Effect
  where
    Load :: Load xs x m (Maybe (Nontrivial xs x))

type instance DispatchOf (Load xs x) = 'Dynamic

load :: Load xs x :> es => Eff es (Maybe (Nontrivial xs x))
load = send Load

-- ⭕ The Step effect

data Step (mo :: Mode) (p :: Perfection) (xs :: Type) (x :: Type) (e :: Type) :: Effect
  where
    Commit :: Positive Natural -> Step 'ReadWrite p xs x m e AdvanceResult
    Next :: Step mo p xs x m e (Maybe (Nontrivial xs x))
    Reset :: Step mo p xs x m e ()
    Fail :: e -> Step mo 'Imperfect xs x e m ()

data Perfection = Perfect | Imperfect

data Mode = ReadOnly | ReadWrite

type instance DispatchOf (Step mo p xs x e) = 'Dynamic

runStep :: forall xs x e es a. '[Load xs x, Buffer 'CommitBuffer xs x, Error e] :>> es => NT.DropOperation xs x
    -> Eff (Step 'ReadWrite 'Imperfect xs x e ': es) a -> Eff es a
runStep dropOp = reinterpret runStepHandler (stepHandler dropOp)

runStepHandler :: forall xs x es a. Buffer 'CommitBuffer xs x :> es => Eff (Buffer 'ViewBuffer xs x ': es) a -> Eff es a
runStepHandler a = getBufferSeq @'CommitBuffer @xs @x >>= \b -> runBuffer' @'ViewBuffer @xs @x b a

stepHandler :: forall xs x es e. '[Load xs x, Buffer 'CommitBuffer xs x, Error e] :>> es => NT.DropOperation xs x
    -> EffectHandler (Step 'ReadWrite 'Imperfect xs x e) (Buffer 'ViewBuffer xs x ': es)
stepHandler dropOp = \_env act -> case act of
      Fail e -> throwError e
      Reset -> getBufferSeq @'CommitBuffer @xs @x >>= putBufferSeq @'ViewBuffer @xs @x
      Next -> runStepNext
      Commit n -> runStepCommit @xs @x dropOp n

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
