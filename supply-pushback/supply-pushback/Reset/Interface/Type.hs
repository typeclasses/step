module Reset.Interface.Type
  (
    {- * Types -} Reset (..), Step (..),
  )
  where

import Next.Interface (Step (..), TerminableStream (..))

import qualified Next

data Reset item product =
    ( product ~ Step item ) => Next
  | ( product ~ ()        ) => Reset

instance TerminableStream item (Reset item) where
    liftNext Next.Next = Next
