module APL.InterpSim (runEval) where

import APL.Monad

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

-- | Continue execution of the provided computation as far as
-- possible, but executing at most one 'StepOp' effect. Any nested
-- computations (in 'BothOp' and 'OneOfOp') must also be stepped
-- similarly. If the computation is stuck on a 'KvGetOp' for which the
-- key is not in the state, then the computation is merely returned
-- unchanged.
--
-- Evaluation of 'BothOp':
--
-- * If either of the nested computations are 'Free (ErrorOp ...)',
--   then propagate that error.
--
-- * If both are 'Pure', then return a pair of the results.
--
-- * Otherwise evaluate both one step.
--
-- Evaluation of 'OneOfOp':
--
-- * If both of the nested computations are 'Free (ErrorOp ...)', then
--   propagate one of the errors.
--
-- * If one is 'Pure', then return that result.
--
-- * Otherwise evaluate both one step.
step :: Env -> State -> EvalM a -> (EvalM a, State)
step _ s (Pure x) = (Pure x, s)
step r s (Free (ReadOp k)) = (k r, s)
step _ s (Free (ErrorOp e)) = (Free (ErrorOp e), s)
step _ s (Free (StepOp m)) = (m, s)
step _ s (Free (KvGetOp key k)) =
  case lookup key s of
    Just val -> (k val, s)
    Nothing -> (Free (KvGetOp key k), s)
step _ s (Free (KvPutOp key val m')) =
  (m', (key, val) : filter (\(k, _) -> k /= key) s)
step r s (Free (BothOfOp m1 m2 k)) =
  case (m1, m2) of
    (Pure v1, Pure v2) -> (k (ValTuple [v1, v2]), s)
    (Free (ErrorOp e), _) -> (Free (ErrorOp e), s)
    (_, Free (ErrorOp e)) -> (Free (ErrorOp e), s)
    _ -> let (m1', s') = step r s m1
             (m2', s'') = step r s' m2
         in (Free (BothOfOp m1' m2' k), s'')
step r s (Free (OneOfOp m1 m2 k)) =
  case (m1, m2) of
    (Pure v1, _) -> (k v1, s)
    (_, Pure v2) -> (k v2, s)
    (Free (ErrorOp e1), Free (ErrorOp _)) -> (Free (ErrorOp e1), s)
    _ -> let (m1', s') = step r s m1
             (m2', s'') = step r s' m2
         in (Free (OneOfOp m1' m2' k), s'')

runEval :: EvalM a -> Either Error a
runEval = go envEmpty stateInitial
  where
    go r s comp = case step r s comp of
      (Pure x, _) -> Right x
      (Free (ErrorOp e), _) -> Left e
      (m', s') -> go r s' m'
