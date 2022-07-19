module Step.Advancement where

import Step.Internal.Prelude

data AdvanceResult = Success | InsufficientInput{ shortfall :: Positive Natural }

class Progressive m where
    advance :: Positive Natural -> m AdvanceResult

instance (Monad m, Progressive m) => Progressive (ReaderT r m) where
    advance n = lift (advance n)
