module Cursor.Atom.Type
  (
    Atom, AtomPlus (..),
  )
  where

import Cursor.Interface

import Data.Either (Either)
import Cursor.Reader.Type (ReaderPlus (..))

newtype AtomPlus up action block error product = Atom
  ( ReaderPlus up action 'Read block
    ( Either error (ReaderPlus up action 'Write block product)
    )
  )

type Atom action block error product =
    forall up. AtomPlus up action block error product
