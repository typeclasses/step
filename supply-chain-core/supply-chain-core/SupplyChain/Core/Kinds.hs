module SupplyChain.Core.Kinds (Interface, Action, Type) where

import Data.Kind (Type)

{-| The kind of requests and responses exchanged between a vendor and a job

    If a job's upstream interface is @i@, then when the job makes a
    request of type @i x@, it receives a response of type @x@.

    Values of a type of this kind represent requests. Each constructor will
    typically have a constraint that specifies what type of response is
    expected in return. Types of this kind are therefore often
    <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/gadt.html GADTs>.
    Types of this kind are also often not functors.

    The lack of any interface at all can be expressed as
    'SupplyChain.Core.Nil.NoInterface'.
-}
type Interface = Type -> Type

{-| A monadic context such as 'System.IO.IO'

    The lack of any actions at all can be expressed as
    'SupplyChain.Core.Nil.NoAction'.
-}
type Action = Type -> Type
