module Step.Action where

-- ⭕

class Is act1 act2 => LossOfMovement act1 act2 | act1 -> act2

instance LossOfMovement Any Any

instance LossOfMovement Atom Atom

instance LossOfMovement Sure Sure

instance LossOfMovement Query Query

instance LossOfMovement Move Any

instance LossOfMovement AtomicMove Atom

instance LossOfMovement Fail Fail

instance LossOfMovement SureQuery SureQuery

-- ⭕

class Is act2 act1 => AssumeSuccess act1 act2 | act1 -> act2 where
    assumeSuccess :: Monad m => act1 xs x e m a -> act2 xs x e m a

instance AssumeSuccess (Step 'ReadOnly 'Imperfect) (Step 'ReadOnly 'Perfect) where
    assumeSuccess = \case
        Step_Commit x -> case x of {}
        Step_Lift x -> Step_Lift x
        Step_Next x -> Step_Next x
        Step_Reset x -> Step_Reset x
        Step_Fail _ -> error "assumeSuccess: assumption failed"

instance AssumeSuccess (Step 'ReadWrite 'Imperfect) (Step 'ReadWrite 'Perfect) where
    assumeSuccess = \case
        Step_Commit x -> Step_Commit x
        Step_Lift x -> Step_Lift x
        Step_Next x -> Step_Next x
        Step_Reset x -> Step_Reset x
        Step_Fail _ -> error "assumeSuccess: assumption failed"

instance AssumeSuccess Any Sure where
    assumeSuccess (Any (ResettingSequence x)) = Sure (ResettingSequence (hoistF assumeSuccess x))

instance AssumeSuccess Query SureQuery where
    assumeSuccess (Query (ResettingSequence x)) = SureQuery (ResettingSequence (hoistF assumeSuccess x))

-- ⭕

-- | Loop0 act1 act2 means that a repetition of 0 or more act1 actions results in an act2 action.
class (Join act1 act2, Possible act2, Is (act1 >> act2) act2) =>
    Loop0 act1 act2 | act1 -> act2

-- Atomic actions loose their atomicity when sequenced 2 or more times; guaranteed advancement is lost when sequencing 0 times

instance Loop0 Atom Any
instance Loop0 AtomicMove Any
instance Loop0 Move Any

-- Other kinds are preserved

instance Loop0 Any Any
instance Loop0 Sure Sure
instance Loop0 SureQuery SureQuery
instance Loop0 Query Query

-- ⭕

-- | Loop1 act1 act2 means that a repetition of 1 or more act1 actions results in an act2 action.
class (Join act1 act2, Is act1 act2, Is (act1 >> act2) act2) =>
    Loop1 act1 act2 | act1 -> act2

-- Atomic actions loose their atomicity when sequenced 2 or more times

instance Loop1 Atom Any
instance Loop1 AtomicMove Move

-- All other kinds are preserved by sequencing

instance Loop1 Any Any
instance Loop1 Query Query
instance Loop1 Move Move
instance Loop1 Sure Sure
instance Loop1 SureQuery SureQuery

-- while :: Monad m => LossOfMovement act1 act2 => Nontrivial.GeneralSpanOperation xs x
--     -> act1 xs x e m a -> act2 xs x e m a
-- while = _

count0 :: forall act1 act2 xs x e m a. Monad m =>
    Loop0 act1 act2 => Natural -> act1 xs x e m a -> act2 xs x e m [a]
count0 = \n a -> go a n
  where
    go a = fix \r -> \case
        0 -> success []
        n -> castTo @act2 $
            a `bindAction` \x -> r (n - 1) <&> \xs -> x : xs

count1 :: forall act1 act2 xs x e m a. Monad m => Loop1 act1 act2 =>
    Positive Natural -> act1 xs x e m a -> act2 xs x e m (NonEmpty a)
count1 = \n a -> go a n
  where
    go a = fix \r -> \p ->
        case preview positive (review positive p - 1) of
            Nothing -> (:| []) <$> castTo @act2 a
            Just p' -> castTo @act2 $
                a `bindAction` \x -> r p' <&> \xs -> NonEmpty.cons x xs

repetition0 :: Monad m => AtomicMove xs x e m a -> Sure xs x e m [a]
repetition0 p = fix \r ->
    try p `bindAction` \case
        Nothing -> return []
        Just x -> (x :) <$> r

repetition1 :: Monad m => AtomicMove xs x e m a -> AtomicMove xs x e m (NonEmpty a)
repetition1 p = p `bindAction` \x -> repetition0 p <&> \xs -> x :| xs

-- ⭕

newtype CursorPosition = CursorPosition{ cursorPositionNatural :: Natural }
    deriving newtype (Eq, Ord, Show, Num)

strictlyIncreaseCursorPosition :: Positive Natural -> Endo CursorPosition
strictlyIncreaseCursorPosition = increaseCursorPosition . review Positive.refine

increaseCursorPosition :: Natural -> Endo CursorPosition
increaseCursorPosition x = Endo $ CursorPosition . (+ x) . cursorPositionNatural

counting :: IsResettingSequence act => MonadState s m => Lens' s CursorPosition -> act xs x r m a -> act xs x r m a
counting o = mapSteps \case
    s@(Step_Commit (Commit n _)) -> do
        liftF (Step_Lift (modifying o $ appEndo $ strictlyIncreaseCursorPosition n))
        liftF s
    s -> liftF s
