module ListStop where

data Confidence = Backtrack | Commit

newtype ListT m a = ListT { next :: m (Confidence, Step m a) }

data Step m a = Cons a (ListT m a) | Nil

deriving stock instance Functor m => Functor (ListT m)

instance Monad m => Applicative (ListT m) where
    pure x = ListT (return (Backtrack, Cons x empty))

    ListT m <*> l = ListT (do
        s <- m
        case s of
            (c, Nil)  -> return (c, Nil)
            (Cons f l' -> next (fmap f l <|> (l' <*> l)) )

    ListT m *> l = ListT (do
        s <- m
        case s of
            Nil       -> return Nil
            Cons _ l' -> next (l <|> (l' *> l)) )

    ListT m <* l = ListT (do
        s <- m
        case s of
            Nil       -> return Nil
            Cons x l' -> next ((x <$ l) <|> (l' <* l)) )

demo :: IO (Int, Int, Int, Int)
demo = runListT @IO do
  (commitA, a) <- select [1,2,3]
  Relude.print a
  (commitB, b) <- select [4,5]
  Relude.print b
  guard (a * b == 8)
  commitA; commitB
  (commitC, c) <- select [7,8,9]
  Relude.print c
  (commitD, d) <- select [10,11,12]
  Relude.print d
  guard (c * d == 88)
  commitC; commitD
  return (a, b, c, d)
