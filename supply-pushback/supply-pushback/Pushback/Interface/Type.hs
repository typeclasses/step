module Pushback.Interface.Type
  (
    Pushback (..),
    Step (..),
  )
  where

import Next.Interface (Step (..), TerminableStream (..))

import qualified Next

data Pushback item product =
    ( product ~ Step item ) => Next
  | ( product ~ ()        ) => Push item

instance TerminableStream item (Pushback item) where
    liftNext Next.Next = Next
