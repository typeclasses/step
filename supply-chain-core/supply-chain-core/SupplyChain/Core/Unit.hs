-- | Description: /unit/ is a simple interface with one request and a fixed
--                response type

module SupplyChain.Core.Unit
  (
    {- * Type -} Unit (Unit),
  )
  where

{-| @Unit a@ is a simple interface: It has a single request
    value (@Unit@), and a fixed response type (@a@). -}
data Unit a b = a ~ b => Unit
