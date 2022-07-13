{-# language DataKinds, FlexibleContexts, KindSignatures, StandaloneKindSignatures #-}
{-# language DerivingVia, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Step.Document.Parser where

import Step.Internal.Prelude

import Step.DocumentMemory.Base (DocumentMemory)
import qualified Step.DocumentMemory.Base as DocumentMemory

import Step.Document.Config (Config)
import qualified Step.Document.Config as Config

import Step.Document.Error (Error (Error))
import qualified Step.Document.Error as Error

import Step.LineHistory.Char (Char)

import Step.ActionTypes (ActionKind, FunctorialAction, MonadicAction, ActionReader (..), ActionT)
import qualified Step.ActionTypes as Action
import Step.ActionTypes.Unsafe (Any (Any))

import qualified Monad

-- | The kind of 'Parser'
type ParserKind =
    Type              -- ^ @text@   - what type of input chunks the parser cursors through
    -> (Type -> Type) -- ^ @base@   - monadic context
    -> ActionKind     -- ^ @kind@   - what properties the parser guarantees; see "Step.ActionTypes"
    -> Type           -- ^ @value@  - produced upon success
    -> Type

type Parser :: ParserKind

type Parser (text :: Type) (base :: Type -> Type) (kind :: ActionKind) (value :: Type) =
    ActionReader Config Error (StateT (DocumentMemory text base) base) kind value

parse :: Monad m => Action.Is k Any =>
    Config -> Parser text m k a -> StateT (DocumentMemory text m) m (Either Error a)
parse config p =
    Action.cast @Any (Action.runActionReader p config) & \(Any p') -> p' >>= \case
        Left errorMaker -> Left <$> errorMaker
        Right x -> return (Right x)

parseOnly :: Action.Is k Any => Monad m => Char char => ListLike text char =>
    Config -> Parser text m k a -> ListT m text -> m (Either Error a)
parseOnly config p xs = evalStateT (parse config p) (DocumentMemory.fromListT xs)

makeError :: Monad m => Config -> m Error
makeError c = return Error{ Error.context = Config.context c }
