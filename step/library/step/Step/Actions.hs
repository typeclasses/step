{-# language ConstraintKinds, FlexibleContexts, ViewPatterns #-}

module Step.Actions
  (
    satisfyJust, peekCharMaybe, takeCharMaybe,
    failure,
    atEnd,
    some,
    text,
    cursorPosition, position,
    (<?>), contextualize,
  )
  where

import Step.Internal.Prelude

import Step.ActionTypes.Types

import Step.ActionTypes (cast)

import qualified Step.ActionTypes.Unsafe as Action.Unsafe

import Loc (Loc)

import qualified Step.ActionTypes.Do as A

import qualified Text as T

import qualified Step.Nontrivial as Nontrivial
import Step.Nontrivial (Nontrivial)

import Step.Cursor (ReadWriteCursor (..), CursoryText, CursoryChar, curse)
import qualified Step.Cursor as Cursor

import Positive.Unsafe (Positive (PositiveUnsafe))

import qualified ListLike

import Step.Document.Locating (Locating)
import qualified Step.Document.Locating as Locating

import Step.Failure (Fallible, Error)
import qualified Step.Failure as F

import Step.Configuration (Configure, HasContextStack, contextStackLens, Config)
import qualified Step.Configuration as Config

import Step.Input.Counter (KnowsCursorPosition)
import qualified Step.Input.Counter as Counting

import Step.Input.CursorPosition (CursorPosition)

type Cursory m = (ListLike (CursoryText m) (CursoryChar m), Eq (CursoryChar m), Cursor.Cursory m, Fallible m)

takeCharMaybe :: Cursory m => Sure m e (Maybe (CursoryChar m))
takeCharMaybe = Action.Unsafe.Sure $ case curse of
    ReadWriteCursor{ init, input, commit } -> run $ Cursor.next input >>= \case
        Nothing -> return Nothing
        Just x -> commit (PositiveUnsafe 1) $> Just (Nontrivial.head x)

peekCharMaybe :: Cursory m => SureQuery m e (Maybe (CursoryChar m))
peekCharMaybe = Action.Unsafe.SureQuery $ case curse of
    ReadWriteCursor{ init, input } -> run $ Cursor.next input <&> \case
        Nothing -> Nothing
        Just x -> Just (Nontrivial.head x)

satisfyJust :: Cursory m => (CursoryChar m -> Maybe a) -> AtomicMove m (Error m) a
satisfyJust ok = Action.Unsafe.AtomicMove $ case curse of
    ReadWriteCursor{ init, input, commit } -> run $ Cursor.next input >>= \case
        Just (ok . Nontrivial.head -> Just x) -> commit (PositiveUnsafe 1) $> Right x
        _ -> return (Left F.failure)

atEnd :: Cursory m => SureQuery m e Bool
atEnd = Action.Unsafe.SureQuery $ case curse of
    ReadWriteCursor{ init, input } -> run $ Cursor.next input <&> isNothing

cursorPosition :: KnowsCursorPosition m => SureQuery m e CursorPosition
cursorPosition = Action.Unsafe.SureQuery Counting.cursorPosition

position :: Locating m => SureQuery m e Loc
position = Action.Unsafe.SureQuery Locating.position

failure :: Cursory m => Fail m (Error m) a
failure = Action.Unsafe.Fail F.failure

some :: Cursory m => AtomicMove m (Error m) (Nontrivial (CursoryText m) (CursoryChar m))
some = Action.Unsafe.AtomicMove $ case curse of
    ReadWriteCursor{ init, input, commit } -> run $ Cursor.next input >>= \case
        Nothing -> return (Left F.failure)
        Just x -> commit (Nontrivial.length x) $> Right x

configure :: Configure m => Action.Unsafe.ChangeBase act =>
    (Config m -> Config m) -> act m e a -> act m e a
configure f = Action.Unsafe.changeBase (Config.configure f)

contextualize :: Configure m => HasContextStack (Config m) => Action.Unsafe.ChangeBase act =>
    T.Text -> act m e a -> act m e a
contextualize n = configure (over contextStackLens (n :))

infix 0 <?>
(<?>) :: Configure m => HasContextStack (Config m) => Action.Unsafe.ChangeBase act =>
    act m e a -> T.Text -> act m e a
p <?> c = contextualize c p

-- todo: add an atomic version of 'text'

text :: Cursory m => Nontrivial (CursoryText m) (CursoryChar m) -> Move m (Error m) ()
text = someOfNontrivialText A.>=> (maybe (return ()) (cast @Any . text) . Nontrivial.refine)
  where
    someOfNontrivialText x = Action.Unsafe.AtomicMove $ case curse of
        ReadWriteCursor{ init, input, commit } -> run $ Cursor.next input >>= \case
            Nothing -> return (Left F.failure)
            Just y ->
                if x `Nontrivial.isPrefixOf` y
                then commit (Nontrivial.length x) $> Right ListLike.empty
                else
                if y `Nontrivial.isPrefixOf` x
                then commit (Nontrivial.length y) $>
                      Right
                        (
                          ListLike.drop
                              (ListLike.length (Nontrivial.generalize y))
                              (Nontrivial.generalize x)
                        )
                else return (Left F.failure)
