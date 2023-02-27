module Reset.Reader.Type
  (
    Reader, ReaderPlus (..),
  )
  where

import Essentials
import SupplyChain
import Reset.Interface

import qualified Control.Monad as Monad

{-| A job with a resettable upstream interface, with the additional implication
    that a resetting sequence is implicitly preceded and followed by a 'reset'

Sequencing operations like '(<*>)' and '(>>=)' insert resets between the operations.
(The implicit resets and the idempotency of 'reset' are essential to arguing that
the 'Applicative' and 'Monad' class laws are sufficiently respected.) -}
newtype ReaderPlus up action item product =
    Reader{ reader :: ResetStream item up => Job up action product }
    deriving stock Functor

type Reader action item product =
    ReaderPlus (Reset item) action item product

instance Applicative (ReaderPlus up action item) where
    pure x = Reader (pure x)
    (<*>) = Monad.ap

instance Monad (ReaderPlus up action item) where
    step1 >>= step2 = Reader do
        x <- reader step1
        order reset
        reader (step2 x)
