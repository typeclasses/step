module Cursor.Atom.Utilities
  (
    unAtom, whileSuccessful, firstSuccess0, firstSuccess1, optional,
  )
  where

import Essentials
import Cursor.Atom.Type
import Cursor.Reader.Type
import Cursor.Interface

import Control.Monad.Trans (lift)
import Control.Monad.Except (ExceptT, runExceptT, catchError, throwError)
import Data.Either (Either (..), either)
import Data.List.NonEmpty (NonEmpty ((:|)))

unAtom :: AtomPlus up action item block error product
    -> ExceptT error (ReaderPlus up action 'Write item block) product
unAtom (Atom x) = x >>= lift

optional :: AtomPlus up action item block error product
    -> ReaderPlus up action 'Write item block (Maybe product)
optional = unAtom >>> runExceptT >>> fmap (either (\_ -> Nothing) Just)

whileSuccessful :: forall up action item block error product. Monoid product =>
    AtomPlus up action item block error product
    -> ReaderPlus up action 'Write item block (error, product)
whileSuccessful (Atom x) = proceed mempty
  where
    proceed :: product -> ReaderPlus up action 'Write item block (error, product)
    proceed as =
        runExceptT x >>= \case
            Left e -> pure (e, as)
            Right rw -> do
                a <- rw
                proceed (as <> a)

firstSuccess0 :: forall up action item block error product. Monoid error =>
    [] (AtomPlus up action item block error product)
    -> AtomPlus up action item block error product
firstSuccess0 xs = Atom $ proceed mempty xs
  where
    proceed e = \case
        [] -> throwError e
        (Atom x' : xs') -> catchError x' \e' -> proceed (e <> e') xs'

firstSuccess1 :: forall up action item block error product. Semigroup error =>
    NonEmpty (AtomPlus up action item block error product)
    -> AtomPlus up action item block error product
firstSuccess1 (Atom x :| xs) = Atom $ catchError x (\e -> proceed e xs)
  where
    proceed e = \case
        [] -> throwError e
        (Atom x' : xs') -> catchError x' \e' -> proceed (e <> e') xs'
