module Step.Input.Buffering where

class Buffering m where

    -- | Fill the buffer to at least one character, if possible
    fillBuffer1 :: m ()

    -- | Read one chunk of input, if possible
    bufferMore :: m ()
