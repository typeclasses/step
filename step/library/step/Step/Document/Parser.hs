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

import Step.Action.Kinds (ActionKind, Any (Any))
import Step.Action.Lift (ActionLift, actionLiftTo)
import Step.Action.UnifiedType (IsAction)
import Step.Action.Join (ActionJoin, actionJoin)
import qualified Step.Action.UnifiedType as Action
import Step.Action.KindJoin ((:>))

import Step.LineHistory.Char (Char)

import Step.Action.Functor (FunctorAction, MonadAction)

import qualified Monad

newtype Parser (text :: Type) (kind :: ActionKind) (m :: Type -> Type) (a :: Type) =
    Parser (kind (Config text) (DocumentMemory text m) (Error text) m a)

instance (Functor m, FunctorAction k) => Functor (Parser text k m) where
    fmap f = Parser . fmap f . (\(Parser a) -> a)

instance (Monad m, MonadAction k) => Applicative (Parser text k m) where
    pure = Parser . pure
    (<*>) = Monad.ap

instance (Monad m, MonadAction k) => Monad (Parser text k m) where
    a >>= b = Parser ((>>=) ((\(Parser x) -> x) a) (fmap ((\(Parser x) -> x)) b))

parse :: Monad m => ActionLift k Any =>
    Config text -> Parser text k m a -> StateT (DocumentMemory text m) m (Either (Error text) a)
parse config (Parser p) =
    actionLiftTo @Any p & \(Any p') ->
    p' config >>= \case
        Left errorMaker -> Left <$> errorMaker
        Right x -> return (Right x)

parseOnly ::  ActionLift k Any => Monad m => Char char => ListLike text char =>
    Config text -> Parser text k m a -> ListT m text -> m (Either (Error text) a)
parseOnly config p xs = evalStateT (parse config p) (DocumentMemory.fromListT xs)

makeError :: Monad m => Config text -> m (Error text)
makeError c = return Error{ Error.context = Config.context c }
