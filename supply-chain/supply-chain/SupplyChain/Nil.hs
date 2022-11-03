module SupplyChain.Nil (NoInterface, NoAction, Const) where

import SupplyChain.Core.Kinds
import qualified SupplyChain.Core.Nil as Nil

type NoInterface = Nil.NoInterface :: Interface

type NoAction = Nil.NoAction :: Action

type Const = Nil.Const :: Type -> Type -> Type
