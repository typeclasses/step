{-# language GeneralizedNewtypeDeriving #-}

module Step.Buffer.Buffer
  (
    Buffer,
    chunks,
  )
  where

import Step.Internal.Prelude

import Step.Nontrivial (Nontrivial)

newtype Buffer xs x = Buffer{ toSeq :: Seq (Nontrivial xs x) }
    deriving stock (Eq, Ord, Show)
    deriving newtype (Semigroup, Monoid)

instance IsList (Buffer xs x) where
    type Item (Buffer xs x) = Nontrivial xs x
    fromList = Buffer . fromList
    toList = toList . toSeq

chunks :: Iso (Buffer xs x) (Buffer xs1 x1) (Seq (Nontrivial xs x)) (Seq (Nontrivial xs1 x1))
chunks = iso toSeq Buffer
