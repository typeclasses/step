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

instance Functor m => Functor (Parser text 'Committing1 m)
  where
    fmap f (AnyParser p) = AnyParser $ anyParser' $ fmap f $ AnyParser' p

instance Monad m => Applicative (Parser text 'Committing1 m)
  where
    pure x = AnyParser $ anyParser' $ pure x
    (AnyParser f) <*> (AnyParser x) = AnyParser $ anyParser' $ AnyParser' f <*> AnyParser' x

instance Monad m => Monad (Parser text 'Committing1 m)
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

class Is a b where generalize :: Monad m => Parser text a m r -> Parser text b m r

instance Is 'Any 'Any where generalize (AnyParser p) = AnyParser p
instance Is 'Committing1 'Committing1 where generalize (AnyParser p) = AnyParser p
instance Is 'Backtracking1 'Backtracking1 where generalize (AnyParser p) = AnyParser p

instance Is 'Committing 'Any where generalize (AnyParser p) = AnyParser p
instance Is 'Backtracking 'Any where generalize (AnyParser p) = AnyParser p
instance Is 'Backtracking1 'Any where generalize (AnyParser p) = AnyParser p
instance Is 'Committing1 'Any where generalize (AnyParser p) = AnyParser p
instance Is 'Failure 'Any where generalize (AnyParser p) = AnyParser p
instance Is 'Certainty0 'Any where generalize (CertainParser p) = AnyParser \config -> Right <$> p config
instance Is 'Certainty1 'Any where generalize (CertainParser p) = AnyParser \config -> Right <$> p config
instance Is 'Certainty 'Any where generalize (CertainParser p) = AnyParser \config -> Right <$> p config

instance Is 'Committing 'Committing
instance Is 'Backtracking 'Committing
instance Is 'Backtracking1 'Committing
instance Is 'Committing1 'Committing
instance Is 'Certainty0 'Committing
instance Is 'Certainty1 'Committing
instance Is 'Certainty 'Committing
instance Is 'Failure 'Committing

instance Is 'Backtracking1 'Committing1
instance Is 'Certainty1 'Committing1
instance Is 'Failure 'Committing1

parseOnly :: Monad m => Is pt 'Any => ListLike text Char => Config text -> Parser text pt m a -> ListT m text -> m (Either (Error text) a)
parseOnly config p xs = evalStateT (parse config p) (DocumentMemory.fromListT xs)
