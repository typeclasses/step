module Cursor.Reader.Type
  (
    Reader, ReaderPlus (..), Atom, AtomPlus (..),
  )
  where

import Essentials
import SupplyChain
import Cursor.Interface
import Block.Class

import qualified Control.Monad as Monad

{-| A job with a resettable upstream interface, with the additional implication
    that a resetting sequence is implicitly preceded and followed by a 'reset'

Sequencing operations like '(<*>)' and '(>>=)' insert resets between the operations.
(The implicit resets and the idempotency of 'reset' are essential to arguing that
the 'Applicative' and 'Monad' class laws are sufficiently respected.) -}
newtype ReaderPlus up action mode block product =
    Reader{ reader ::
        Block block => IsCursor mode block up =>
            Job up action product }
    deriving stock Functor

type Reader action mode block product =
    ReaderPlus (Cursor mode block) action mode block product

instance Applicative (ReaderPlus up action mode block) where
    pure x = Reader (pure x)
    (<*>) = Monad.ap

instance Monad (ReaderPlus up action mode block) where
    step1 >>= step2 = Reader do
        x <- reader step1
        order reset
        reader (step2 x)

newtype AtomPlus up action block fallible product =
    Atom (ReaderPlus up action 'Read block (fallible (ReaderPlus up action 'Write block product)))

type Atom action block fallible product =
    forall up. AtomPlus up action block fallible product
