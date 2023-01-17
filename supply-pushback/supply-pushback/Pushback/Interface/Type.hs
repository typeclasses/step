module Pushback.Interface.Type where

import Next.Interface (Step (..), TerminableStream (..))

import qualified Next

data Pushback item product =
    ( product ~ Step item ) => Next
  | ( product ~ ()        ) => Push item

instance TerminableStream block (Pushback block) where
    liftNext Next.Next = Next
