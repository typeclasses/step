module Block.BlockBlock.Type
  (
    BlockBlock (BlockBlock),
  )
  where

import Essentials
import Block.Class

import Data.List.NonEmpty (NonEmpty (..))
import Data.String (IsString (..), String)
import Integer (Positive)
import Integer.Signed (Signed (..))
import Prelude ((+))
import Data.Bool ((&&))
import GHC.Exts (IsList (..), Item)
import Prelude (error)
import Data.List.NonEmpty (nonEmpty)
import Fold.ShortcutNonempty (ShortcutNonemptyFold)

import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Fold.Nonempty as Fold
import qualified Integer.Positive as Positive
import qualified Fold.ShortcutNonempty as ShortcutFold

data BlockBlock (x :: Type) (xs :: Type) (xss :: Type) =
    BlockBlockUnsafe{ bbXss :: !xss, bbLength :: !Positive }
    deriving stock (Eq, Ord, Show)


---  Pattern  ---

pattern BlockBlock :: forall x xs xss. (Block x xs, Block xs xss) =>
    xss -> BlockBlock x xs xss
pattern BlockBlock xss <- BlockBlockUnsafe xss _
  where
    BlockBlock xss = BlockBlockUnsafe xss
      (Fold.run Fold.sum (length <$> (toNonEmpty Front xss :: NonEmpty xs)))

{-# complete BlockBlock #-}


---  IsList  ---

instance (Block x xs, Block xs xss) => IsList (BlockBlock x xs xss) where

    type Item (BlockBlock x xs xss) = x

    toList =
        toNonEmpty Front >>> Foldable.toList

    fromList =
        nonEmpty
        >>> Maybe.fromMaybe (error "fromList BlockBlock: empty")
        >>> fromNonEmpty Front


---  Semigroup  ---

instance (Block x xs, Block xs xss) => Semigroup (BlockBlock x xs xss) where
    (<>) = (++)


---  IsString  ---

instance (IsString xs, Block x xs, Block xs xss) =>
  IsString (BlockBlock x xs xss) where

    fromString :: String -> BlockBlock x xs xss
    fromString = fromString >>> singleton >>> BlockBlock


---  Block  ---

instance (Block x xs, Block xs xss) => Block x (BlockBlock x xs xss)


---  ItemEquality  ---

instance (Block x xs, Block xs xss, ItemEquality xs) =>
        ItemEquality (BlockBlock x xs xss) where

    sameItems :: BlockBlock x xs xss -> BlockBlock x xs xss -> Bool
    sameItems = \(BlockBlockUnsafe xss1 len1) (BlockBlockUnsafe xss2 len2) ->
                    len1 == len2 && go xss1 xss2
      where
        end = Front
        go :: xss -> xss -> Bool
        go (pop end -> Pop xs1 mXss1) (pop end -> Pop xs2 mXss2) =
            case biPrefix itemEquality end (xs1, xs2) of
                NoPrefixRelation -> False
                BothPrefix -> case (mXss1, mXss2) of
                    (Nothing, Nothing) -> True
                    (Just xss1, Just xss2) -> go xss1 xss2
                    _ -> False
                IsPrefix First _ remainder -> case mXss1 of
                    Nothing -> False
                    Just xss1 -> go xss1 (unpop end (Pop remainder mXss2))
                IsPrefix Second _ remainder -> case mXss2 of
                    Nothing -> False
                    Just xss2 -> go (unpop end (Pop remainder mXss1)) xss2


---  Concat  ---

instance (Block x xs, Block xs xss) => Concat (BlockBlock x xs xss) where

    (++) :: BlockBlock x xs xss -> BlockBlock x xs xss -> BlockBlock x xs xss
    BlockBlockUnsafe xss1 len1 ++ BlockBlockUnsafe xss2 len2 =
        BlockBlockUnsafe (xss1 ++ xss2) (len1 + len2)

    concat :: End -> NonEmpty (BlockBlock x xs xss) -> BlockBlock x xs xss
    concat end bbs = BlockBlockUnsafe (concat end (bbs <&> bbXss)) (Fold.run Fold.sum (bbs <&> bbLength))


---  Enumerate  ---

instance (Block x xs, Block xs xss) => Enumerate x (BlockBlock x xs xss) where

    foldItems :: End -> ShortcutNonemptyFold x a -> BlockBlock x xs xss -> a
    foldItems end f@(ShortcutNonemptyFold{ ShortcutFold.step }) = bbXss >>>
        foldItems end ShortcutNonemptyFold
          { ShortcutFold.initial = foldItems end f
          ,  \xs ->
            modify \a -> foldItems end (execState a . stepX) stepX xs

    toNonEmpty :: End -> BlockBlock x xs xss -> NonEmpty x
    toNonEmpty end = bbXss >>>
        \(pop end -> Pop (pop end -> Pop x xs) xss) -> x :| f xs xss
      where

        f :: Maybe xs -> Maybe xss -> [x]
        f Nothing xss = g xss
        f (Just (pop end -> Pop x xs)) xss = x : f xs xss

        g :: Maybe xss -> [x]
        g Nothing = []
        g (Just (pop end -> Pop (pop end -> Pop x xs) xss)) =
            x : f xs xss

instance (Block x xs, Block xs xss) => Construct x (BlockBlock x xs xss) where

    fromNonEmpty :: End -> NonEmpty x -> BlockBlock x xs xss
    fromNonEmpty end = fromNonEmpty end >>> singleton >>> BlockBlock

instance (Block x xs, Block xs xss) => Singleton x (BlockBlock x xs xss) where

    singleton :: x -> BlockBlock x xs xss
    singleton x = BlockBlockUnsafe (singleton (singleton x)) 1

    pop :: End -> BlockBlock x xs xss -> Pop x (BlockBlock x xs xss)
    pop end (BlockBlockUnsafe xss n) = Pop x remainder
      where
        Pop xs xssMaybe = pop end xss
        Pop x  xsMaybe  = pop end xs
        remainder = pushMaybe end xsMaybe xssMaybe
            <&> \xss' -> BlockBlockUnsafe xss'
              (n & Positive.subtractOne & Positive.fromNatural & Maybe.fromJust)

    push :: End -> x -> BlockBlock x xs xss -> BlockBlock x xs xss
    push end x (BlockBlockUnsafe xss n) =
        BlockBlockUnsafe (push end (singleton x) xss) (n + 1)

instance (Block x xs, Block xs xss) => Positional (BlockBlock x xs xss) where

    length :: BlockBlock x xs xss -> Positive
    length = bbLength

    take :: End -> Positive -> BlockBlock x xs xss -> Take (BlockBlock x xs xss)
    take end n (BlockBlockUnsafe xss len) = case Positive.subtract len n of
        Minus s -> TakeInsufficient (Shortfall s)
        Zero -> TakeAll
        Plus remainderLength ->
            TakePart
                (BlockBlockUnsafe (Maybe.fromJust taken) n)
                (BlockBlockUnsafe (Maybe.fromJust remainder) remainderLength)
          where
            (taken, remainder) = evalState n (find end f xss) & Maybe.fromJust
                & \Pivot{ pivot = (x1, x2), split1 = (_, xs2), split2 = (xs1, _) } ->
                    (pushMaybe (oppositeEnd end) x1 xs1, pushMaybe end x2 xs2)
              where
                f xs = do
                    rem <- get
                    case take end rem xs of
                        TakeInsufficient (Shortfall s)  ->  put s $> Nothing
                        TakeAll                         ->  pure $ Just (Just xs, Nothing)
                        TakePart a b                    ->  pure $ Just (Just a, Just b)

instance (Block x xs, Block xs xss) => Index x (BlockBlock x xs xss) where

    at :: End -> Positive -> BlockBlock x xs xss -> Maybe x
    at end = \n (BlockBlock xss) -> go n xss
      where
        go :: Positive -> xss -> Maybe x
        go n xss = case Positive.subtract (length xs) n of
            Minus n' -> xss' & maybe Nothing (go n')
            NotMinus _ -> at end n xs
          where
            Pop xs xss' = pop end xss

instance (Block x xs, Block xs xss) => Search x (BlockBlock x xs xss) where

    find :: End -> (x -> State s (Maybe found)) -> BlockBlock x xs xss
        -> State s (Maybe (Pivot found (BlockBlock x xs xss)))
    find end f bb =
        bb & bbXss
        & find end (find end f)
        <&> fmap \Pivot{ pivot = Pivot{ pivot = x
                                      , split1 = (a1', b')
                                      , split2 = (a', b1')
                                      }
                       , split1 = (_, b)
                       , split2 = (a, _)
                       } ->
                Pivot
                  { pivot = x
                  , split1 =
                      ( BlockBlock $ unpop (oppositeEnd end) (Pop a1' a)
                      , BlockBlock <$> pushMaybe end b' b
                      )
                  , split2 =
                      ( BlockBlock <$> pushMaybe (oppositeEnd end) a' a
                      , BlockBlock $ unpop end (Pop b1' b)
                      )
                  }
