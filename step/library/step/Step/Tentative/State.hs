module Step.Tentative.State where

import Step.Internal.Prelude

import Step.Tentative.Base (Tentative (Tentative))
import qualified Step.Tentative.Base as Tentative

ifJust :: Monad m => (a -> Maybe b) -> Tentative s m a -> StateT s m (Maybe b)
ifJust f (Tentative g) = do
    s <- get
    step <- lift (g s)
    case Tentative.newState step of
        Tentative.NoChoice s' -> put s' $> f (Tentative.result step)
        Tentative.NewStateChoice c -> do
            let r = f (Tentative.result step)
            put ((if isJust r then Tentative.ifActionTaken else Tentative.ifNotTaken) c)
            return r
