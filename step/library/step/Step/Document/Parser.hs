{-# language DataKinds, FlexibleContexts, KindSignatures, StandaloneKindSignatures #-}
{-# language DerivingVia, StandaloneDeriving #-}

module Step.Document.Parser where

import Step.Internal.Prelude

import Step.DocumentMemory.Base (DocumentMemory)
import qualified Step.DocumentMemory.Base as DocumentMemory

import Step.Document.Config (Config)
import qualified Step.Document.Config as Config

import Step.Document.Error (Error (Error))
import qualified Step.Document.Error as Error

import Step.LineHistory.Char (Char)

import Step.ActionTypes (ActionKind, FunctorialAction, MonadicAction)
import qualified Step.ActionTypes as Action (Is, cast)
import Step.ActionTypes.Unsafe (Any (Any))

import qualified Monad

-- | The kind of 'Parser'
type ParserKind =
    Type              -- ^ @text@   - what type of input chunks the parser cursors through
    -> ActionKind     -- ^ @kind@   - what properties the parser guarantees; see "Step.ActionTypes"
    -> (Type -> Type) -- ^ @base@   - monadic context
    -> Type           -- ^ @value@  - produced upon success
    -> Type

type Parser :: ParserKind

newtype Parser (text :: Type) (kind :: ActionKind) (base :: Type -> Type) (value :: Type) =
    Parser (Config -> kind (DocumentMemory text base) Error base value)

-- | Parser is a Functor for every 'ActionKind'.
deriving
    via (ReaderT Config (kind (DocumentMemory text base) Error base))
    instance (Functor base, FunctorialAction kind) => Functor (Parser text kind base)

-- | Parser is only Applicative + Monadic for certain action kinds; see 'MonadAction'
deriving
    via (ReaderT Config (kind (DocumentMemory text base) Error base))
    instance (Monad base, MonadicAction kind) => Applicative (Parser text kind base)

-- | Parser is only Applicative + Monadic for certain action kinds; see 'MonadAction'
deriving
    via (ReaderT Config (kind (DocumentMemory text base) Error base))
    instance (Monad base, MonadicAction kind) => Monad (Parser text kind base)

-- | Convert a parser's 'ActionKind' to something more general; see "Step.ActionTypes"
cast :: forall k2 k1 text m a. Monad m => Action.Is k1 k2 =>
    Parser text k1 m a -> Parser text k2 m a
cast (Parser p) = Parser (Action.cast @k2 @k1 . p)

parse :: Monad m => Action.Is k Any =>
    Config -> Parser text k m a -> StateT (DocumentMemory text m) m (Either Error a)
parse config (Parser p) =
    Action.cast @Any (p config) & \(Any p') -> p' >>= \case
        Left errorMaker -> Left <$> errorMaker
        Right x -> return (Right x)

parseOnly :: Action.Is k Any => Monad m => Char char => ListLike text char =>
    Config -> Parser text k m a -> ListT m text -> m (Either Error a)
parseOnly config p xs = evalStateT (parse config p) (DocumentMemory.fromListT xs)

makeError :: Monad m => Config -> m Error
makeError c = return Error{ Error.context = Config.context c }
