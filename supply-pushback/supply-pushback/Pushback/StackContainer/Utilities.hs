module Pushback.StackContainer.Utilities
  (
    {- * Utilities -} zoom,
  )
  where

import Essentials
import Pushback.StackContainer.Type

import qualified Optics as O

zoom :: O.Lens' context container
    -> StackContainer container item
    -> StackContainer context item
zoom containerLens StackContainer{ push, pop } = StackContainer
    { push = \item context -> O.over containerLens (push item) context
    , pop = \context ->
        let container = O.view containerLens context
        in  pop container <&> \(x, container') ->
                (x, O.set containerLens container' context)
    }
