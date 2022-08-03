{-# language GeneralizedNewtypeDeriving, Trustworthy #-}

module Step.Buffer
  (
    {- * Definition -} Buffer, chunks,
    {- * Operations -} takeChunk, dropFromBuffer,
  )
  where

import Step.Internal.Prelude

import Step.Nontrivial (Nontrivial, DropOperation (..), Drop (..))
import Step.Cursor (AdvanceResult (..))
import Step.RST (RST (..))

newtype Buffer xs x = Buffer{ toSeq :: Seq (Nontrivial xs x) }
    deriving newtype (Semigroup, Monoid)

instance IsList (Buffer xs x) where
    type Item (Buffer xs x) = Nontrivial xs x
    fromList = Buffer . fromList
    toList = toList . toSeq

chunks :: Iso (Buffer xs x) (Buffer xs1 x1) (Seq (Nontrivial xs x)) (Seq (Nontrivial xs1 x1))
chunks = iso toSeq Buffer

takeChunk :: Monad m => RST r (Buffer xs x) m (Maybe (Nontrivial xs x))
takeChunk = use chunks >>= \case
    Empty -> return Nothing
    y :<| ys -> assign chunks ys $> Just y

dropFromBuffer :: Monad m =>
    DropOperation xs x
    -> Positive Natural
    -> RST r (Buffer xs x) m AdvanceResult
dropFromBuffer DropOperation{ drop } = fix \r n -> use chunks >>= \case
    Empty -> return YouCanNotAdvance{ shortfall = n }
    x :<| xs -> case drop n x of
        DropAll -> assign chunks xs $> AdvanceSuccess
        DropPart{ dropRemainder } -> assign chunks (dropRemainder :<| xs) $> AdvanceSuccess
        DropInsufficient{ dropShortfall } -> assign chunks xs *> r dropShortfall
