module CommitList where

data CommitListT m a = CommitListT { next :: m (Step m a) }

data Step m a = Offer a (CommitListT m a) | Nil

deriving stock instance Functor m => Functor (CommitListT m)

deriving stock instance Functor m => Functor (Step m)

runCommitListT :: Monad m => CommitListT m a -> m ()
runCommitListT (CommitListT m) = do
    s <- m
    case s of
        Nil -> return ()
        Offer _ more -> runCommitListT more



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
