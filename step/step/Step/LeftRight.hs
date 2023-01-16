module Step.LeftRight
  (
    LeftRight (..),
    MaybeRight (..),
    Either (Left, Right),
    Right (Right', getRight),
  )
  where

import Essentials

import Data.Either (Either (Left, Right))

class LeftRight x where toEither :: x l r -> Either l r
class MaybeRight x where right :: r -> x l r

newtype Right e a = Right'{ getRight :: a }

instance LeftRight Either where toEither = id
instance MaybeRight Either where right = Right

instance LeftRight Right where toEither = Right . getRight
instance MaybeRight Right where right = Right'
