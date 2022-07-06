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

import Step.Action.Kinds (ActionKind)
import qualified Step.Action.Kinds as T
import qualified Step.Action.SeparateTypes as T
import Step.Action.Lift (ActionLift, actionLiftTo)
import Step.Action.UnifiedType (IsAction, ActionJoin, actionJoin)
import qualified Step.Action.UnifiedType as Action
import Step.Action.KindJoin ((:>))

import Step.LineHistory.Char (Char)

import Step.Action.Functor (FunctorAction)

import qualified Monad

newtype Parser (text :: Type) (kind :: ActionKind) (m :: Type -> Type) (a :: Type) =
    Parser (kind (Config text) (DocumentMemory text m) (Error text) m a)

instance (Functor m, FunctorAction k) => Functor (Parser text k m) where
    fmap f = Parser . fmap f . (\(Parser a) -> a)

instance (Monad m, FunctorAction k, T.MonadAction k) => Applicative (Parser text k m) where
    pure = Parser . T.pureAction
    (<*>) = Monad.ap

instance (Monad m, FunctorAction k, T.MonadAction k) => Monad (Parser text k m) where
    a >>= b = Parser (T.bindAction ((\(Parser x) -> x) a) (fmap ((\(Parser x) -> x)) b))

parse :: Monad m => ActionLift k T.Any =>
    Config text -> Parser text k m a -> StateT (DocumentMemory text m) m (Either (Error text) a)
parse config (Parser p) =
    actionLiftTo @T.Any p & \(T.Any p') ->
    StateT (p' config) >>= \case
        Left errorMaker -> Left <$> errorMaker
        Right x -> return (Right x)

parseOnly ::  ActionLift k T.Any => Monad m => Char char => ListLike text char =>
    Config text -> Parser text k m a -> ListT m text -> m (Either (Error text) a)
parseOnly config p xs = evalStateT (parse config p) (DocumentMemory.fromListT xs)

makeError :: Monad m => Config text -> m (Error text)
makeError c = return Error{ Error.context = Config.context c }
