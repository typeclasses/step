module Monitor.Recording.Type
  (
    {- * Type -} Recording (..),
  )
  where

import SupplyChain (Job)

data Recording up monitor state action = Recording
    { initial :: state -- ^ Initial state
    , step :: forall a. up a -> a -> state -> Job up action state
        -- ^ How to update state when a new request/response is seen
    , extract :: forall a. monitor a -> state -> Job up action a
        -- ^ How to respond to serve responses on the monitor interface
    }
