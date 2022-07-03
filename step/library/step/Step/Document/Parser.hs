module Step.Document.Parser where

import Step.Internal.Prelude hiding (Is)

import Step.DocumentMemory.Base (DocumentMemory)
import qualified Step.DocumentMemory.Base as DocumentMemory
import qualified Step.DocumentMemory.State as DocumentMemory.State

import Step.Document.Config (Config)
import qualified Step.Document.Config as Config

import Step.Document.Error (Error (Error))
import qualified Step.Document.Error as Error

import Loc (Loc)

import Step.Kind.Base (StepKind (..), FallibilityOf, Fallibility (..))

data Parser (text :: Type) (pt :: StepKind) (m :: Type -> Type) (a :: Type)
  where
    AnyParser :: FallibilityOf pt ~ 'MightFail =>
        (Config text -> StateT (DocumentMemory text m) m (Either (Error text) a))
        -> Parser text pt m a
    CertainParser :: FallibilityOf pt ~ 'AlwaysSucceeds =>
        (Config text -> StateT (DocumentMemory text m) m a)
        -> Parser text pt m a

instance Functor m => Functor (Parser text 'Any m)
  where
    fmap f (AnyParser p) = AnyParser $ anyParser' $ fmap f $ AnyParser' p

instance Monad m => Applicative (Parser text 'Any m)
  where
    pure x = AnyParser $ anyParser' $ pure x
    (AnyParser f) <*> (AnyParser x) = AnyParser $ anyParser' $ AnyParser' f <*> AnyParser' x

instance Monad m => Monad (Parser text 'Any m)
  where
    AnyParser x >>= f = AnyParser $ anyParser' $ AnyParser' x >>= (\(AnyParser y) -> AnyParser' y) . f

newtype AnyParser' text m a = AnyParser'{ anyParser' :: (Config text -> StateT (DocumentMemory text m) m (Either (Error text) a)) }
    deriving (Functor, Applicative, Monad)
        via (ReaderT (Config text) (ExceptT (Error text) (StateT (DocumentMemory text m) m)))

instance Functor m => Functor (Parser text 'Certainty0 m)
  where
    fmap f (CertainParser p) = CertainParser $ certainParser' $ fmap f $ CertainParser' p

instance Monad m => Applicative (Parser text 'Certainty0 m)
  where
    pure x = CertainParser $ certainParser' $ pure x
    (CertainParser f) <*> (CertainParser x) = CertainParser $ certainParser' $ CertainParser' f <*> CertainParser' x

instance Monad m => Monad (Parser text 'Certainty0 m)
  where
    CertainParser x >>= f = CertainParser $ certainParser' $ CertainParser' x >>= (\(CertainParser y) -> CertainParser' y) . f

newtype CertainParser' text m a = CertainParser'{ certainParser' :: (Config text -> StateT (DocumentMemory text m) m a) }
    deriving (Functor, Applicative, Monad)
        via (ReaderT (Config text) (StateT (DocumentMemory text m) m))

-- deriving via (ReaderT (Config text) (ExceptT (Error text) (StateT (DocumentMemory text m) m)))
--     instance Functor (Parser text 'Any m)

makeError :: Config text -> Error text
makeError config = Error{ Error.context = Config.context config }

parse :: Monad m => Is pt 'Any => Config text -> Parser text pt m a -> StateT (DocumentMemory text m) m (Either (Error text) a)
parse config p = let AnyParser p' = generalizeTo @'Any p in p' config

-- generalize :: forall pt2 pt1 m text. (pt1 `Is` pt2) => Monad m =>
--        Parser text pt1 m a
--     -> Parser text pt2 m a
-- generalize = \case
--     AnyParser p -> AnyParser p
--     CertainParser p -> AnyParser \config -> Right <$> p config

generalizeTo :: forall b a text m r. Monad m => Is a b => Parser text a m r -> Parser text b m r
generalizeTo = generalize @a @b

class Bind pt1 pt2 pt3 | pt1 pt2 -> pt3 where
    bind :: Parser text pt1 m a
          -> (a -> Parser text pt2 m b)
          -> Parser text pt3 m b

instance Bind 'Backtracking1 'Certainty0 'Committing1 where

instance Bind 'Backtracking1 'Committing1 'Committing1 where

instance Bind 'Committing1 'Committing1 'Committing1 where

instance Bind 'Backtracking1 'Any 'Committing1 where

instance Bind 'Committing1 'Any 'Committing1 where

instance Bind 'Any 'Any 'Any where

class Is pt1 pt2 where
    generalize :: Monad m => Parser text pt1 m a -> Parser text pt2 m a

instance Is 'Any 'Any where

instance Is 'Committing1 'Any where

instance Is 'Backtracking1 'Any where

instance Is 'Backtracking1 'Committing1 where

instance Is 'Certainty 'Any where

instance Is 'Certainty0 'Any

instance Is 'Certainty0 'Certainty

to :: forall pt2 pt1 text m a. Is pt1 pt2 => Monad m => Parser text pt1 m a -> Parser text pt2 m a
to = generalize

parseOnly :: Is pt 'Any => Monad m => ListLike text Char => Config text -> Parser text pt m a -> ListT m text -> m (Either (Error text) a)
parseOnly config p xs = evalStateT (parse config p) (DocumentMemory.fromListT xs)
