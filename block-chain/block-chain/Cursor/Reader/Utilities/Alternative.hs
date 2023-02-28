module Cursor.Reader.Utilities.Alternative where

import Essentials
import Cursor.Reader.Type

import Cursor.Interface (Mode (..))
import Data.Either (Either (..))
import Control.Monad.Trans (lift)
import Control.Monad.Except (ExceptT, throwError, mapExceptT)
import Optics (over, _Left)

import qualified Cursor.Atom as Atom

{-| Run the first reader in the list that can produce a 'Just' result -}
firstJust :: [Reader action 'Write item block (Maybe product)]
    -> ReaderPlus up action 'Write item block (Maybe product)
firstJust = fmap Atom.just >>> Atom.firstSuccess0 >>> Atom.optional

{-| Repeat a reader as long as it keeps returning 'Just',
    combining the results monoidally -}
whileJust :: Monoid product =>
    Reader action 'Write item block (Maybe product)
    -> ReaderPlus up action 'Write item block product
whileJust = Atom.just >>> Atom.whileSuccessful >>> fmap (\((), x) -> x)

whileSuccessfulJust :: Monoid product =>
    ExceptT error (Reader action 'Write item block) (Maybe product)
    -> ReaderPlus up action 'Write item block (Maybe error, product)
whileSuccessfulJust =
    (mapExceptT $ fmap $ over _Left Just)
    >>> (>>= maybe (throwError Nothing) pure)
    >>> Atom.atom >>> Atom.whileSuccessful

whileJustExcept :: Monoid product =>
    ExceptT error (Reader action 'Write item block) (Maybe product)
    -> ExceptT error (ReaderPlus up action 'Write item block) product
whileJustExcept = whileSuccessfulJust >>> lift >>>
    (>>= \(em, x) -> maybe (pure x) throwError em)

{-| Repeat a reader as long as it keeps returning 'Right',
    returning the first 'Left' result and the monoidally
    accumulated 'Right' results -}
whileRight :: Monoid product =>
    Reader action 'Write item block (Either conclusion product)
    -> ReaderPlus up action 'Write item block (conclusion, product)
whileRight = Atom.right >>> Atom.whileSuccessful

{-| Repeat a fallible reader as long as it keeps succeeding, returning
    the first error and the monoidally accumulated successes -}
whileSuccessful :: Monoid product =>
    ExceptT error (Reader action 'Write item block) product
    -> ReaderPlus up action 'Write item block (error, product)
whileSuccessful = Atom.atom >>> Atom.whileSuccessful
