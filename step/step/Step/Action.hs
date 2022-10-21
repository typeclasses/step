module Step.Action
  (
    {- * Actions -} Action, {- $types -}
      Any, Query, Sure, SureQuery, Atom (..), Move, AtomicMove, Failure (..),

    {- * Classes -} IsAction (..), Atomic (..), AssumeMovement (..), IsResettingSequence (..), run, act,

    {- * Subtyping (Is, castTo) -} {- $subtyping -} Is (..), castTo,

    {- * Composition -}
    {- ** Type family (>>) -} type (>>),
    {- ** Monad-like operations -} {- $do -} Join (..), bindAction,

  )
  where

import Step.Action.Core

{- $do

For additional Monad-like operations, see "Step.Do" and consider using the
<https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/qualified_do.html QualifiedDo>
language extension.

-}
