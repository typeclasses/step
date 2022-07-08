{-# language DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Step.Tentative.Base where

-- | Similar to 'StateT', but offers a choice of two possible new states.
newtype Tentative s m a = Tentative (s -> m (Step s a))

data Step s a = Step{ newState :: NewState s, result :: a }

data NewState s = NewStateChoice (Choice s) | NoChoice s

data Choice s = Choice{ ifNotTaken :: s, ifActionTaken :: s }

noChoiceStep :: s -> a -> Step s a
noChoiceStep s a = Step{ result = a, newState = NoChoice s }

choiceStep :: Choice s -> a -> Step s a
choiceStep s a = Step{ result = a, newState = NewStateChoice s }
