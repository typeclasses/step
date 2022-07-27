module Step.Buffer.DoubleBuffer
  (
    {- * Type -} DoubleBuffer,
    {- * Creation -} newDoubleBuffer,
    {- * Optics -} uncommitted, unseen,
  )
  where

import Step.Buffer.DoubleBuffer.Constructor (DoubleBuffer)
import Step.Buffer.DoubleBuffer.Optics (uncommitted, unseen)
import Step.Buffer.DoubleBuffer.Extra (newDoubleBuffer)
