module Step.Buffer.Buffer where

import Data.Foldable (toList)

-- Containers
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

newtype Buffer c = Buffer{ bufferSeq :: Seq c }

pattern One :: a -> Buffer a
pattern One x = Buffer (x Seq.:<| Seq.Empty)

pattern (:<) :: c -> Buffer c -> Buffer c
pattern x :< xs <- Buffer (x Seq.:<| (Buffer -> xs))
  where
    x :< Buffer xs = Buffer (x Seq.:<| xs)

pattern (:>) :: Buffer c -> c -> Buffer c
pattern xs :> x <- Buffer ((Buffer -> xs) Seq.:|> x)
  where
    Buffer xs :> x = Buffer (xs Seq.:|> x)

pattern Empty :: Buffer a
pattern Empty = Buffer Seq.Empty

pattern List :: [a] -> Buffer a
pattern List xs <- Buffer (toList -> xs)
  where
    List xs = Buffer (Seq.fromList xs)

{-# complete Empty, (:>) #-}
{-# complete Empty, (:<) #-}
{-# complete List #-}
