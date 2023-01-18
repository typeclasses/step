module Cursor.Reader.Type where

import Block.Class (Block)
import Cursor.Interface (Cursor, IsCursor)
import SupplyChain (Job)

{-| A job whose upstream interface is 'Cursor'

A good reader should generally 'Reset' at the end. -}
type Reader action mode block product =
    Block block =>
        Job (Cursor mode block) action product

{-| Like 'Reader', but with a polymorphic upstream interface

A good reader should generally 'Reset' at the end. -}
type ReaderPlus up action mode block product =
    Block block => IsCursor mode block up =>
        Job up action product
