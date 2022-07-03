module Step.Kind.Base where

import Bool (Bool (..))

data StepKind =
    Any        -- ^ No known properties
  | Static     -- ^ Does not move the cursor
  | Move       -- ^ Always moves the cursor
  | Undo       -- ^ Fails noncommitally
  | MoveUndo   -- ^ Always moves the cursor, fails noncommitally
  | Sure       -- ^ Always succeeds
  | SureStatic -- ^ Always succeeds, does not move the cursor
  | SureMove   -- ^ Always succeeds, always moves the cursor

data Advancement = Stationary | Advances | MightAdvance

data Fallibility = MightFail | AlwaysSucceeds

data Commitment = Noncommittal | MightCommitFailure

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
type instance AdvancementOf 'Undo = 'MightAdvance
type instance AdvancementOf 'Sure = 'MightAdvance

type instance AdvancementOf 'SureStatic = 'Stationary
type instance AdvancementOf 'Static = 'Stationary

type instance AdvancementOf 'MoveUndo = 'Advances
type instance AdvancementOf 'Move = 'Advances
type instance AdvancementOf 'SureMove = 'Advances

type family FallibilityOf (p :: StepKind) :: Fallibility

type instance FallibilityOf 'Any = 'MightFail
type instance FallibilityOf 'Undo = 'MightFail
type instance FallibilityOf 'MoveUndo = 'MightFail
type instance FallibilityOf 'Move = 'MightFail
type instance FallibilityOf 'Static = 'MightFail

type instance FallibilityOf 'SureStatic = 'AlwaysSucceeds
type instance FallibilityOf 'SureMove = 'AlwaysSucceeds
type instance FallibilityOf 'Sure = 'AlwaysSucceeds

type family CommitmentOf (p :: StepKind) :: Commitment

type instance CommitmentOf 'Any = 'MightCommitFailure
type instance CommitmentOf 'Move = 'MightCommitFailure
type instance CommitmentOf 'Undo = 'Noncommittal
type instance CommitmentOf 'MoveUndo = 'Noncommittal
type instance CommitmentOf 'SureStatic = 'Noncommittal
type instance CommitmentOf 'SureMove = 'Noncommittal
type instance CommitmentOf 'Sure = 'Noncommittal
type instance CommitmentOf 'Static = 'Noncommittal
