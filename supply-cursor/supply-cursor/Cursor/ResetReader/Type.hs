module Cursor.ResetReader.Type where

import Essentials
import SupplyChain
import Cursor.Interface
import Cursor.Reader.Type
import Block.Class

import qualified Control.Monad as Monad

{-| A job with a resettable upstream interface, with the additional implication
    that a resetting sequence is implicitly preceded and followed by a 'reset'

Sequencing operations like '(<*>)' and '(>>=)' insert resets between the operations.
(The implicit resets and the idempotency of 'reset' are essential to arguing that
the 'Applicative' and 'Monad' class laws are sufficiently respected.) -}
newtype ResetReader' up action mode block product =
    ResetReader{ reader :: ReaderPlus up action mode block product }
    deriving stock Functor

type ResetReader action mode block product =
    Block block =>
        ResetReader' (Cursor mode block) action mode block product

type ResetReaderPlus up action mode block product =
    Block block => IsCursor mode block up =>
        ResetReader' up action mode block product

instance IsCursor mode block up => Applicative (ResetReader' up action mode block) where
    pure x = ResetReader (pure x)
    (<*>) = Monad.ap

instance IsCursor mode block up => Monad (ResetReader' up action mode block) where
    step1 >>= step2 = ResetReader do
        x <- reader step1
        order reset
        reader (step2 x)
