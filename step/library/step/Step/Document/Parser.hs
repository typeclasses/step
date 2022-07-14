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

import Step.ActionTypes (Action, FunctorialAction, MonadicAction)
import qualified Step.ActionTypes as Action
import Step.ActionTypes.Unsafe (Any (Any))

import qualified Monad

-- | The kind of 'Parser'
type ParserKind =
    Type              -- ^ @text@   - what type of input chunks the parser cursors through
    -> (Type -> Type) -- ^ @base@   - monadic context
    -> Action         -- ^ @kind@   - what properties the parser guarantees; see "Step.ActionTypes"
    -> Type           -- ^ @value@  - produced upon success
    -> Type

parse :: Monad base => Action.Is kind Any =>
    Config
    -> kind (ReaderT Config (StateT (DocumentMemory text base) base)) Error value
    -> StateT (DocumentMemory text base) base (Either Error value)
parse config p =
    p & Action.cast @Any & \(Any p') -> runReaderT p' config >>= \case
        Left errorMaker -> Left <$> runReaderT errorMaker config
        Right x -> return (Right x)

parseOnly :: Action.Is kind Any => Monad base => Char char => ListLike text char =>
    Config
    -> kind (ReaderT Config (StateT (DocumentMemory text base) base)) Error value
    -> ListT base text -> base (Either Error value)
parseOnly config p xs = evalStateT (parse config p) (DocumentMemory.fromListT xs)

makeError :: Monad m => Config -> m Error
makeError c = return Error{ Error.context = Config.context c }
