{-| The 'Either' interface, a general way to connect a
    job to two vendors by merging them into one -}
module SupplyChain.Interface.Either
  (
    {- * Interface -} Either (..),
    {- * Vendor composition -} offerEither,
  )
  where

import Essentials
import SupplyChain

import qualified SupplyChain.Referral as Referral

-- | Combination of two interfaces
data Either a b response = Left (a response) | Right (b response)

-- | Combination of two vendors
offerEither :: forall up down1 down2 action.
     Vendor up down1 action
  -> Vendor up down2 action
  -> Vendor up (Either down1 down2) action
offerEither a@(Vendor a') b@(Vendor b') = Vendor \case
    Left  req -> a' req <&> \s -> s{ Referral.next = offerEither (Referral.next s) b }
    Right req -> b' req <&> \s -> s{ Referral.next = offerEither a (Referral.next s) }
