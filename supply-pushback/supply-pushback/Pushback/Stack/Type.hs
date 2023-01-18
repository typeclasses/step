module Pushback.Stack.Type
  (
    Stack, StackPlus,
  )
  where

import Pushback.Interface
import Essentials
import SupplyChain

{-| A 'Vendor' whose upstream interface is nothing
    and whose downstream interface is 'Pushback' -}
type Stack action item =
    Vendor (Const Void) (Pushback item) action

{-| A 'Vendor' whose downstream interface is 'Pushback' -}
type StackPlus up action item =
    Vendor up (Pushback item) action
