module Cursor.Atom.Type
  (
    Atom, AtomPlus (..),
  )
  where

import Essentials
import Cursor.Interface

import Data.Either (Either (..))
import Cursor.Reader.Type (ReaderPlus (..))

newtype AtomPlus up action item block error product = Atom
  ( forall mode. ReaderPlus up action mode item block
    ( Either error (ReaderPlus up action 'Write item block product)
    )
  )

type Atom action block error product =
    forall up. AtomPlus up action block error product

instance Semigroup error =>
    Semigroup (AtomPlus up action item block error product)
  where
    Atom a <> Atom b = Atom do
        z1 <- a
        case z1 of
            Right x -> pure (Right x)
            Left e1 -> do
                z2 <- b
                pure case z2 of
                    Right x -> Right x
                    Left e2 -> Left (e1 <> e2)

instance Monoid error =>
    Monoid (AtomPlus up action item block error product)
  where
    mempty = Atom (pure (Left mempty))
