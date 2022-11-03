module SupplyChain.Core.Nil (NoInterface, NoAction,
    Const (Const, getConst)) where

import SupplyChain.Core.Kinds

import Data.Functor.Const (Const (Const, getConst))
import Data.Void (Void)

-- | An 'Interface' that admits no requests
type NoInterface = Const Void :: Interface

-- | An 'Action' that admits no actions
type NoAction = Const Void :: Action
