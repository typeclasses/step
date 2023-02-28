module Cursor.Reader.Type
  (
    {- * Types -} Reader, ReaderPlus (..),
  )
  where

import Essentials
import SupplyChain
import Cursor.Interface
import Block

import qualified Control.Monad as Monad

{-| A job with a resettable upstream interface, with the additional implication
    that a resetting sequence is implicitly preceded and followed by a 'reset'

Sequencing operations like '(<*>)' and '(>>=)' insert resets between the operations.
(The implicit resets and the idempotency of 'reset' are essential to arguing that
the 'Applicative' and 'Monad' class laws are sufficiently respected.) -}
newtype ReaderPlus up action mode item block product =
    Reader{ reader ::
        Block item block => IsCursor mode block up =>
            Job up action product }
    deriving stock Functor

type Reader action mode item block =
    ReaderPlus (Cursor mode block) action mode item block

instance Applicative (ReaderPlus up action mode item block) where
    pure x = Reader (pure x)
    (<*>) = Monad.ap

instance Monad (ReaderPlus up action mode item block) where
    step1 >>= step2 = Reader do
        x <- reader step1
        order reset
        reader (step2 x)
