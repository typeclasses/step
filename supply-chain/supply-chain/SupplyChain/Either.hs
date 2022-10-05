module SupplyChain.Either where

import SupplyChain

import Data.Functor (Functor, (<&>))


-- | Combination of two interfaces

data Either a b response = Left (a response) | Right (b response)

type Either :: Interface -> Interface -> Interface


-- | Combination of two vendors

offerEither :: forall up down1 down2 action. Functor action =>
    Vendor up down1 action -> Vendor up down2 action -> Vendor up (Either down1 down2) action

offerEither a@(Vendor a') b@(Vendor b') = Vendor \case
    Left  req -> a' req <&> \s -> s{ supplyNext = offerEither (supplyNext s) b }
    Right req -> b' req <&> \s -> s{ supplyNext = offerEither a (supplyNext s) }
