{-| A Buffer is a type of Stack (see "Pushback.Stack"), specifically
    one whose upstream interface is a "TerminableStream" -}
module Pushback.Buffer.Type
  (
    Buffer, BufferPlus,
  )
  where

import Next.Interface (TerminableStream, Next)
import Pushback.Interface.Type (Pushback)
import SupplyChain (Vendor)

{-| A 'Vendor' whose upstream interface is 'Next' and whose
    downstream interface is 'Pushback'

Such a vendor can play the role of an adapter that adds pushback
functionality to a stream that doesn't support it. -}
type Buffer action item =
    Vendor (Next item) (Pushback item) action

{-| Like 'Buffer', but with a polymorphic upstream interface -}
type BufferPlus up action item =
    TerminableStream item up =>
        Vendor up (Pushback item) action
