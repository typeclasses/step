module Cursor.Reader.Utilities.Alternative where

import Essentials
import Cursor.Reader.Type

import Cursor.Interface (Mode (..))
import Data.Either (Either (..))
import Data.Foldable (foldMap)
import Control.Monad.Except (ExceptT (ExceptT), throwError)

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

{-| Repeat a reader as long -}
whileRight :: Monoid product =>
    Reader action 'Write item block (Either conclusion product)
    -> ReaderPlus up action 'Write item block (conclusion, product)
whileRight = Atom.right >>> Atom.whileSuccessful

whileSuccessful :: Monoid product =>
    ExceptT error (Reader action 'Write item block) product
    -> ReaderPlus up action 'Write item block (error, product)
whileSuccessful = Atom.atom >>> Atom.whileSuccessful
