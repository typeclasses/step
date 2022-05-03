module CommitList where

import ListT (ListT (ListT), runListT)
import qualified ListT
import qualified Set
import qualified Show

newtype CommitListT (m :: Type -> Type) (a :: Type) =
    CommitListT (ListT (StateT CommitState m) a)
    deriving newtype (Functor, Applicative, Alternative, Monad, MonadIO)

data CommitState =
  CommitState
    { tentativeKeySet :: Set CommitKey
    , nextCommitKey :: CommitKey
    }

instance Show CommitState
  where
    show x = "CommitState " <> show (fmap commitKeyNumber $ Set.toList $ tentativeKeySet x) <> " " <> show (commitKeyNumber $ nextCommitKey x)

newtype CommitKey = CommitKey Natural
    deriving newtype (Eq, Ord)
    deriving stock Show

commitKeyNumber :: CommitKey -> Natural
commitKeyNumber (CommitKey n) = n

overTentativeKeySet :: (Set CommitKey -> Set CommitKey) -> CommitState -> CommitState
overTentativeKeySet f cs = cs{ tentativeKeySet = f (tentativeKeySet cs) }

overNextCommitKey :: (CommitKey -> CommitKey) -> CommitState -> CommitState
overNextCommitKey f cs = cs{ nextCommitKey = f (nextCommitKey cs) }

incrementCommitKey :: CommitKey -> CommitKey
incrementCommitKey (CommitKey n) = CommitKey (succ n)

runCommitListT :: forall m a. Monad m => CommitListT m a -> m ()
runCommitListT (CommitListT list) = evalStateT (runListT list) initialState

initialState :: CommitState
initialState = CommitState{ tentativeKeySet = Set.empty, nextCommitKey = CommitKey 0 }

tentatively :: MonadIO m => CommitListT m a -> CommitListT m (CommitKey, a)
tentatively x = do{ k <- newCommitKey; i <- whileTentative k x; return (k, i) }

newCommitKey :: forall m. MonadIO m => CommitListT m CommitKey
newCommitKey = CommitListT @m $ lift do
    cs <- get
    let k = nextCommitKey cs
    modify' $ overNextCommitKey (const (incrementCommitKey k))
    modify' $ overTentativeKeySet (Set.insert k)
    return k

whileTentative :: forall m a. MonadIO m => CommitKey -> CommitListT m a -> CommitListT m a
whileTentative k = CommitListT @m . go . \(CommitListT x) -> x
  where
    go :: ListT (StateT CommitState m) a -> ListT (StateT CommitState m) a
    go x = do
      cs <- get
      print "+"
      print cs
      ListT $ if Set.member k (tentativeKeySet cs)
          then ListT.next x >>= \case
              ListT.Nil -> return ListT.Nil
              ListT.Cons i y -> do
                  cs' <- get
                  print "*"
                  print cs'
                  if Set.member k (tentativeKeySet cs')
                      then return $ ListT.Cons i (go y)
                      else return ListT.Nil
          else return ListT.Nil

commit :: forall m. Monad m => CommitKey -> CommitListT m ()
commit = CommitListT . modify' . overTentativeKeySet . Set.delete

guardCommit :: forall m. Monad m => Bool -> [CommitKey] -> CommitListT m ()
guardCommit = \case{ True -> traverse_ commit ; False -> const $ ListT.select [] }

demo :: IO ()
demo = runCommitListT @IO $ sequence $ replicate 4 do
    (commitA, a) <- tentatively $ ListT.select [1..40]
    print (a, commitA)
    (commitB, b) <- tentatively $ ListT.select [4,5]
    print (b, commitB)
    guardCommit (a * b == 8) [commitA, commitB]
    (commitC, c) <- tentatively $ ListT.select [7 .. 50]
    print (c, commitC)
    (commitD, d) <- tentatively $ ListT.select [10..12]
    print (d, commitD)
    guardCommit (c * d == 88) [commitC, commitD]
    print "---"
    print (a, b, c, d)
    CommitListT (lift (get >>= print))
    print "---"

--   where
--     CommitListT :: m (Step m a) -> CommitListT m a
--     Tentatively :: CommitKey -> CommitListT m a -> CommitListT m a
--     NewCommitKey :: CommitListT m CommitKey
--     Fmap :: (a -> b) -> CommitListT m a -> CommitListT m b


-- runCommitListT = go (CommitKey 0) Set.empty
--   where
--     go :: CommitKey -> Set CommitKey -> CommitListT m a -> m ()
--     go nextCommitKey uncertainties = \case
--         CommitListT m -> do
--             s <- m
--             case s of
--                 Nil -> return ()
--                 Offer _ more -> go nextCommitKey uncertainties more
--                 Commit k more -> go nextCommitKey (Set.delete k uncertainties) more
--         Tentatively k x -> do
--             _
--         NewCommitKey -> return ()

-- commit :: CommitKey -> CommitListT m a
-- commit = _

-- instance Monad m => Alternative (ListT m) where
--     empty = ListT (return Nil)
--     Commit <|> _ = Commit
--     ListT a <|> ListT b = ListT $ a >>= \case


-- instance Monad m => Applicative (ListT m) where
--     pure x = ListT (return (Cons x empty))

--     ListT m <*> l = ListT (do
--         s <- m
--         case s of
--             (c, Nil)  -> return (c, Nil)
--             (c, Cons f l') -> next (fmap f l <|> (l' <*> l)) )

    -- ListT m *> l = ListT (do
    --     s <- m
    --     case s of
    --         Nil       -> return Nil
    --         Cons _ l' -> next (l <|> (l' *> l)) )

    -- ListT m <* l = ListT (do
    --     s <- m
    --     case s of
    --         Nil       -> return Nil
    --         Cons x l' -> next ((x <$ l) <|> (l' <* l)) )

-- demo :: IO (Int, Int, Int, Int)
-- demo = runListT @IO do
--   (commitA, a) <- select [1,2,3]
--   print a
--   (commitB, b) <- select [4,5]
--   print b
--   guard (a * b == 8)
--   commitA; commitB
--   (commitC, c) <- select [7,8,9]
--   print c
--   (commitD, d) <- select [10,11,12]
--   print d
--   guard (c * d == 88)
--   commitC; commitD
--   return (a, b, c, d)
