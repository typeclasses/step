module Cursor.Atom.Type
  (
    Atom, AtomPlus (..),
  )
  where

import Essentials
import Cursor.Interface

import Cursor.Reader.Type (ReaderPlus (..))
import Control.Monad.Except (ExceptT, throwError, catchError)

newtype AtomPlus up action item block error product = Atom
  ( forall mode.
      ExceptT error
        (ReaderPlus up action mode item block)
        (ReaderPlus up action 'Write item block product)
  )

type Atom action block error product =
    forall up. AtomPlus up action block error product

instance Semigroup error =>
        Semigroup (AtomPlus up action item block error product) where

    Atom a <> Atom b =
        Atom $
            catchError a \e1 ->
            catchError b \e2 ->
            throwError (e1 <> e2)

instance Monoid error => Monoid (AtomPlus up action item block error product) where
    mempty = Atom (throwError mempty)
