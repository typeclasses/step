module Step.Kind.Base where

import Bool (Bool (..))

data StepKind =
    Any
  | Committing
  | Backtracking
  | Backtracking1
  | Committing1
  | Certainty0
  | Certainty1
  | Certainty
  | Fallible0
  | Failure

data Advancement = Stationary | Advances | MightAdvance

data Fallibility = MightFail | AlwaysSucceeds

data Commitment = Noncommittal | MightCommitFailure

type family Join (a :: StepKind) (b :: StepKind) :: StepKind

type instance Join 'Any 'Any = 'Any
type instance Join 'Committing 'Committing = 'Committing
type instance Join 'Backtracking 'Backtracking = 'Committing
type instance Join 'Committing1 'Committing1 = 'Committing1
type instance Join 'Backtracking1 'Backtracking1 = 'Committing1

type instance Join 'Any 'Committing = 'Any
type instance Join 'Committing 'Any = 'Any

type instance Join 'Committing 'Backtracking = 'Committing
type instance Join 'Backtracking 'Committing = 'Committing

type instance Join 'Backtracking1 'Certainty1 = 'Committing1
type instance Join 'Certainty1 'Backtracking1 = 'Committing1

type instance Join 'Certainty0 'Backtracking1 = 'Committing1
type instance Join 'Backtracking1 'Certainty0 = 'Committing1

type instance Join 'Committing1 'Backtracking1 = 'Committing1
type instance Join 'Backtracking1 'Committing1 = 'Committing1

type family CanBeStationary (p :: Advancement) :: Bool

type instance CanBeStationary 'Stationary = 'True
type instance CanBeStationary 'MightAdvance = 'True
type instance CanBeStationary 'Advances = 'False

type family CanAdvance (p :: Advancement) :: Bool

type instance CanAdvance 'Stationary = 'False
type instance CanAdvance 'MightAdvance = 'True
type instance CanAdvance 'Advances = 'True

type family AdvancementOf (p :: StepKind) :: Advancement

type instance AdvancementOf 'Any = 'MightAdvance
type instance AdvancementOf 'Backtracking = 'MightAdvance
type instance AdvancementOf 'Committing = 'MightAdvance
type instance AdvancementOf 'Certainty = 'MightAdvance

type instance AdvancementOf 'Certainty0 = 'Stationary
type instance AdvancementOf 'Failure = 'Stationary
type instance AdvancementOf 'Fallible0 = 'Stationary

type instance AdvancementOf 'Backtracking1 = 'Advances
type instance AdvancementOf 'Committing1 = 'Advances
type instance AdvancementOf 'Certainty1 = 'Advances

type family FallibilityOf (p :: StepKind) :: Fallibility

type instance FallibilityOf 'Any = 'MightFail
type instance FallibilityOf 'Backtracking = 'MightFail
type instance FallibilityOf 'Committing = 'MightFail
type instance FallibilityOf 'Backtracking1 = 'MightFail
type instance FallibilityOf 'Committing1 = 'MightFail
type instance FallibilityOf 'Failure = 'MightFail
type instance FallibilityOf 'Fallible0 = 'MightFail

type instance FallibilityOf 'Certainty0 = 'AlwaysSucceeds
type instance FallibilityOf 'Certainty1 = 'AlwaysSucceeds
type instance FallibilityOf 'Certainty = 'AlwaysSucceeds

type family CommitmentOf (p :: StepKind) :: Commitment

type instance CommitmentOf 'Any = 'MightCommitFailure
type instance CommitmentOf 'Committing = 'MightCommitFailure
type instance CommitmentOf 'Committing1 = 'MightCommitFailure
type instance CommitmentOf 'Backtracking = 'Noncommittal
type instance CommitmentOf 'Backtracking1 = 'Noncommittal
type instance CommitmentOf 'Certainty0 = 'Noncommittal
type instance CommitmentOf 'Certainty1 = 'Noncommittal
type instance CommitmentOf 'Certainty = 'Noncommittal
type instance CommitmentOf 'Failure = 'Noncommittal
type instance CommitmentOf 'Fallible0 = 'Noncommittal
