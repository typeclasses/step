module Step.Package.End where

import Step.Action.Core
import Step.Error
import Step.Walk (Walk (..))
import Step.Package.Failure

import qualified Step.Do as P
import qualified Step.Interface as Interface

import Data.Bool (Bool (..))


atEnd :: SureQuery c m e Bool
atEnd = SureQuery (Walk Interface.atEnd)

end :: forall c m e. ErrorContext e m => Query c m e ()
end = atEnd P.>>= requireTrue
