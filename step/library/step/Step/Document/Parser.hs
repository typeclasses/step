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

import qualified Step.Action.Kind as ActionKind
import Step.Action.Kind (ActionKind)
import Step.Action.Family (Action)
import qualified Step.Action.Family as Action
import qualified Step.Action.Lift as Action

newtype Parser (text :: Type) (kind :: ActionKind) (m :: Type -> Type) (a :: Type) =
    Parser (Action kind (Config text) (DocumentMemory text m) (Error text) m a)

deriving newtype instance Functor m => Functor (Parser text kind m)

deriving newtype instance Monad m => Applicative (Parser text kind m)

deriving newtype instance Monad m => Monad (Parser text kind m)

makeError :: Config text -> Error text
makeError config = Error{ Error.context = Config.context config }

parse :: Monad m => Action.Lift k 'ActionKind.Any => Config text -> Parser text k m a -> StateT (DocumentMemory text m) m (Either (Error text) a)
parse config p = let Action.Any p' = Action.liftTo @'ActionKind.Any p in StateT (p' config)

parseOnly :: Action.Lift k 'ActionKind.Any => Monad m => ListLike text Char => Config text -> Parser text k m a -> ListT m text -> m (Either (Error text) a)
parseOnly config p xs = evalStateT (parse config p) (DocumentMemory.fromListT xs)
