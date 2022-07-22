module Step.Input.Buffering where

class Buffering m where

    -- | Read one chunk of input, if possible
    bufferMore :: m ()
