{- |

The only properties guaranteed by construction are that /Sure/ always succeeds and /Fail/ never does. The rest of the properties are not enforced by constructors. This module is, therefore, unsafe.

-}

module Step.ActionTypes.Unsafe
  (
    Any (..),
    Atom (..),
    AtomicMove (..),
    Fail (..),
    Move (..),
    Query (..),
    Sure (..),
    SureQuery (..),
  )
  where

import Step.ActionTypes.Constructors
