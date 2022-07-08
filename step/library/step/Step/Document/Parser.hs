module Step.Document.Parser where

import Step.Internal.Prelude

import Step.DocumentMemory.Base (DocumentMemory)
import qualified Step.DocumentMemory.Base as DocumentMemory

import Step.Document.Config (Config)
import qualified Step.Document.Config as Config

import Step.Document.Error (Error (Error))
import qualified Step.Document.Error as Error

import Step.LineHistory.Char (Char)

import Step.Action.Safe (ActionKind, FunctorAction, MonadAction)
import qualified Step.Action.Safe as Action

import Step.Action.Constructors (Any (Any))

import qualified Monad

-- | The kind of 'Parser'
type ParserKind =
    Type              -- ^ @text@   - what type of input chunks the parser cursors through
    -> ActionKind     -- ^ @kind@   - what properties the parser guarantees; see "Step.Action.Types"
    -> (Type -> Type) -- ^ @base@   - monadic context
    -> Type           -- ^ @value@  - produced upon success
    -> Type

type Parser :: ParserKind

newtype Parser (text :: Type) (kind :: ActionKind) (base :: Type -> Type) (value :: Type) =
    Parser (kind (Config text) (DocumentMemory text base) (Error text) base value)

-- | Parser is a Functor for every 'ActionKind'.
deriving newtype instance (Functor base, FunctorAction k) => Functor (Parser text k base)

-- | Parser is only Applicative + Monadic for certain action kinds; see 'MonadAction'
deriving newtype instance (Monad base, MonadAction k) => Applicative (Parser text k base)

-- | Parser is only Applicative + Monadic for certain action kinds; see 'MonadAction'
deriving newtype instance (Monad base, MonadAction k) => Monad (Parser text k base)

-- | Convert a parser's 'ActionKind' to something more general; see "Step.Action.Subtyping"
--
cast :: forall k2 k1 text m a. Monad m => Action.Is k1 k2 =>
    Parser text k1 m a -> Parser text k2 m a
cast = under (iso Parser (\(Parser z) -> z)) (Action.cast @k2 @k1)

parse :: Monad m => Action.Is k Any =>
    Config text -> Parser text k m a -> StateT (DocumentMemory text m) m (Either (Error text) a)
parse config (Parser p) =
    Action.cast @Any p & \(Any p') ->
    p' config >>= \case
        Left errorMaker -> Left <$> errorMaker
        Right x -> return (Right x)

parseOnly :: Action.Is k Any => Monad m => Char char => ListLike text char =>
    Config text -> Parser text k m a -> ListT m text -> m (Either (Error text) a)
parseOnly config p xs = evalStateT (parse config p) (DocumentMemory.fromListT xs)

makeError :: Monad m => Config text -> m (Error text)
makeError c = return Error{ Error.context = Config.context c }
