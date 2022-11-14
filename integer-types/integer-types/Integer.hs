{-# language Safe #-}

module Integer
  (
    {- ** Types -}
    Integer, Natural, Positive,
    Signed (Zero, NonZero, Minus, Plus),
    Sign (MinusSign, PlusSign),

    {- ** Subtraction -}
    Subtraction (subtractInteger, subtractSigned),
    Subtraction' (subtract),

    {- ** Conversion classes -}
    IntegerNarrow (narrow),
    IntegerConvert (convert),
    IntegerEquiv,
  )
  where

import Integer.Integer (Integer)
import Integer.Natural (Natural)
import Integer.Positive (Positive)

import Integer.Sign (Sign (MinusSign, PlusSign))
import Integer.Signed (Signed (Zero, NonZero, Minus, Plus))

import Integer.Conversion (IntegerNarrow (narrow),
    IntegerConvert (convert), IntegerEquiv)

import Integer.Subtraction (
    Subtraction (subtractInteger, subtractSigned),
    Subtraction' (subtract))
