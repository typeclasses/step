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

import Step.Action.SeparateTypes (ActionKind)
import qualified Step.Action.SeparateTypes as T
import Step.Action.UnifiedType (Action (..), ActionLift, actionLiftTo, IsAction, actionIso, ActionJoin, (:>))
import qualified Step.Action.UnifiedType as Action

import Variado.Monad.Class

newtype Parser (text :: Type) (kind :: ActionKind) (m :: Type -> Type) (a :: Type) =
    Parser (Action kind (Config text) (DocumentMemory text m) (Error text) m a)
    deriving newtype (Functor, Applicative, Monad)
    -- deriving (Functor, Configurable (Config text))
    --     via (Action kind (Config text) (DocumentMemory text m) (Error text) m)

instance (Monad m, IsAction kind1, IsAction kind2, ActionJoin kind1 kind2) => PolyMonad (Parser text kind1 m) (Parser text kind2 m)
  where
    type Join (Parser text kind1 m) (Parser text kind2 m) = Parser text (kind1 :> kind2) m
    join (Parser a) = Parser (join (fmap (\(Parser b) -> b) a))

action :: Iso
    (Parser text1 kind m1 a1)
    (Parser text2 kind m2 a2)
    (Action kind (Config text1) (DocumentMemory text1 m1) (Error text1) m1 a1)
    (Action kind (Config text2) (DocumentMemory text2 m2) (Error text2) m2 a2)
action = coerced

action' :: IsAction kind => Iso
    (Parser text1 kind m1 a1)
    (Parser text2 kind m2 a2)
    (kind (Config text1) (DocumentMemory text1 m1) (Error text1) m1 a1)
    (kind (Config text2) (DocumentMemory text2 m2) (Error text2) m2 a2)
action' = coerced % re actionIso

-- instance PolyMonad

-- instance (Functor m) => Functor (Parser text kind m)
--   where
--     fmap = coerce fmapAction

-- deriving newtype instance Functor m => Functor (Parser text kind m)

-- deriving newtype instance Monad m => Applicative (Parser text kind m)

-- deriving newtype instance Monad m => Monad (Parser text kind m)

makeError :: Config text -> Error text
makeError config = Error{ Error.context = Config.context config }

parse :: Monad m => ActionLift k T.Any => Config text -> Parser text k m a -> StateT (DocumentMemory text m) m (Either (Error text) a)
parse config (Parser p) = let Action.Any (T.Any p') = actionLiftTo @T.Any p in StateT (p' config)

parseOnly :: ActionLift k T.Any => Monad m => ListLike text Char => Config text -> Parser text k m a -> ListT m text -> m (Either (Error text) a)
parseOnly config p xs = evalStateT (parse config p) (DocumentMemory.fromListT xs)
