module Pushback.Stack.Type where

import Pushback.Interface
import Essentials
import SupplyChain

type Stack action item =
    Vendor (Const Void) (Pushback item) action

type StackPlus up action item =
    Vendor up (Pushback item) action
