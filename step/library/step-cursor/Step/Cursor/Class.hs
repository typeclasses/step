module Step.Cursor.Class
  (
    Cursory (..),
  )
  where

import Step.Internal.Prelude

import Step.Cursor.Type

class Cursory m where
    type Text m :: Type
    type Char m :: Type
    curse :: Cursor (Text m) (Char m) m
