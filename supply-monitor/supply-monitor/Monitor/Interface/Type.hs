module Monitor.Interface.Type
  (
    {- * Type -} Monitor (..),
  )
  where

data Monitor monitor up product =
    Subject (up product)
      -- ^ The interface of the subject being monitored
  | Monitor (monitor product)
      -- ^ The interface added by the monitoring system
