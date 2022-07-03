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

data Advancement = Stationary | Advances | MightAdvance

data Fallibility = MightFail | AlwaysSucceeds

data Commitment = Noncommittal | MightCommitFailure

data ParserType =
    Any
  | Backtracking
  | Committing
  | Backtracking1
  | Committing1
  | Certainty0
  | Certainty1
  | Certainty
  | Failure

type family AdvancementOf (p :: ParserType) :: Advancement

type instance AdvancementOf 'Any = 'MightAdvance
type instance AdvancementOf 'Backtracking = 'MightAdvance
type instance AdvancementOf 'Committing = 'MightAdvance
type instance AdvancementOf 'Certainty = 'MightAdvance

type instance AdvancementOf 'Certainty0 = 'Stationary
type instance AdvancementOf 'Failure = 'Stationary

type instance AdvancementOf 'Backtracking1 = 'Advances
type instance AdvancementOf 'Committing1 = 'Advances
type instance AdvancementOf 'Certainty1 = 'Advances

type family FallibilityOf (p :: ParserType) :: Fallibility

type instance FallibilityOf 'Any = 'MightFail
type instance FallibilityOf 'Backtracking = 'MightFail
type instance FallibilityOf 'Committing = 'MightFail
type instance FallibilityOf 'Backtracking1 = 'MightFail
type instance FallibilityOf 'Committing1 = 'MightFail
type instance FallibilityOf 'Failure = 'MightFail

type instance FallibilityOf 'Certainty0 = 'AlwaysSucceeds
type instance FallibilityOf 'Certainty1 = 'AlwaysSucceeds
type instance FallibilityOf 'Certainty = 'AlwaysSucceeds

type family CommitmentOf (p :: ParserType) :: Commitment

type instance CommitmentOf 'Any = 'MightCommitFailure
type instance CommitmentOf 'Committing = 'MightCommitFailure
type instance CommitmentOf 'Committing1 = 'MightCommitFailure
type instance CommitmentOf 'Backtracking = 'Noncommittal
type instance CommitmentOf 'Backtracking1 = 'Noncommittal
type instance CommitmentOf 'Certainty0 = 'Noncommittal
type instance CommitmentOf 'Certainty1 = 'Noncommittal
type instance CommitmentOf 'Certainty = 'Noncommittal
type instance CommitmentOf 'Failure = 'Noncommittal


data Parser (text :: Type) (pt :: ParserType) (m :: Type -> Type) (a :: Type)
  where
    AnyParser ::
        (Config text -> StateT (DocumentMemory text m) m (Either (Error text) a))
        -> Parser text pt m a
    CertainParser :: FallibilityOf pt ~ 'AlwaysSucceeds =>
        (Config text -> StateT (DocumentMemory text m) m a)
        -> Parser text pt m a

makeError :: Config text -> Error text
makeError config = Error{ Error.context = Config.context config }

parse :: Monad m => Config text -> Parser text pt m a -> StateT (DocumentMemory text m) m (Either (Error text) a)
parse config p = let AnyParser p' = generalize p in p' config

generalize :: Monad m =>
       Parser text pt m a
    -> Parser text 'Any m a
generalize = \case
    AnyParser p -> AnyParser p
    CertainParser p -> AnyParser \config -> Right <$> p config

parseOnly :: Monad m => ListLike text Char => Config text -> Parser text pt m a -> ListT m text -> m (Either (Error text) a)
parseOnly config p xs = evalStateT (parse config p) (DocumentMemory.fromListT xs)


-- -- Each parser type belongs to one of the following two classes:

-- class CanConsume0 (p :: ParserKind) where trivial :: Monad m => a -> p text m a
-- instance CanConsume0 Parser where trivial = return
-- instance CanConsume0 Certainty where trivial = return
-- deriving via Parser instance CanConsume0 Possibility
-- deriving via Parser instance CanConsume0 Parser0

-- class ConsumesAtLeast1 (p :: ParserKind) where
-- instance ConsumesAtLeast1 Parser1
-- instance ConsumesAtLeast1 Certainty1
-- instance ConsumesAtLeast1 Possibility1


-- -- Parser types that admit failure:

-- class CanFail (p :: ParserKind) where failure :: Monad m => p text m a
-- instance CanFail Parser where failure = Parser (\config -> return (Left (makeError config)))
-- deriving via Parser instance CanFail Parser0
-- deriving via Parser instance CanFail Parser1
-- deriving via Parser instance CanFail Possibility
-- deriving via Parser instance CanFail Possibility1


-- -- Parser types that can obtain the document position

-- class HasPosition (p :: ParserKind) where position :: Monad m => ListLike text Char => p text m Loc
-- instance HasPosition Parser where position = Parser \_config -> Right <$> DocumentMemory.State.getPosition
-- deriving via Parser instance HasPosition Parser1
-- deriving via Parser instance HasPosition Possibility
-- deriving via Parser instance HasPosition Possibility1
-- instance HasPosition Certainty where position = Certainty \_config -> DocumentMemory.State.getPosition
