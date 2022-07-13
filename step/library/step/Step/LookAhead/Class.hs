{-# language FlexibleInstances, FunctionalDependencies #-}

module Step.LookAhead.Class where

import Step.Internal.Prelude

import Step.ActionTypes

class Monad m => LookAhead m text | m -> text where

    peekCharMaybe :: ListLike text char => m (Maybe char)

    atEnd :: ListLike text char => m Bool
    atEnd = isNothing <$> peekCharMaybe

-- instance LookAhead m text => LookAhead (SureQuery e m) text
