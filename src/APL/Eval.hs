module APL.Eval
  ( eval,
  )
where

import APL.AST (Exp (..), VName)
import APL.Monad

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f e1 e2 =
  evalIntBinOp f' e1 e2
  where
    f' x y = pure $ f x y


-- One could just pass i and p as arguments however if they are updated somewhere in should get this.
forLoop :: VName -> VName -> Integer -> Exp -> EvalM Val
forLoop p_var i_var n body = do
  p <- eval (Var p_var)
  i <- eval (Var i_var)
  case i of
    ValInt i' -> loop p i'
    _ -> failure "Loop index must be an integer"
  where
    loop p i
      | i >= n = pure p
      | otherwise = do
          localEnv (envExtend p_var p . envExtend i_var (ValInt i)) $ do
            p' <- eval body
            loop p' (i + 1)

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "If: non-boolean conditional"
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (Tuple es)
  | length es < 2 = failure "Tuple must contain at least 2 elements"
  | otherwise = do
      vs <- mapM eval es
      pure $ ValTuple vs
eval (Project e i) = do
  v <- eval e
  case v of
    ValTuple vs -> 
      if i < 0 || i >= fromIntegral (length vs)
        then failure "Tuple index out of bounds"
        else pure $ vs !! fromIntegral i
    _ -> failure "Cannot project non-tuple"
eval (KvPut key_e val_e) = do
  key <- eval key_e
  val <- eval val_e
  evalKvPut key val
  pure val
eval (KvGet key_e) = do
  key <- eval key_e
  evalKvGet key
eval (BothOf e1 e2) = undefined
eval (OneOf e1 e2) = undefined

-- Partly written by ChatGPT
eval (ForLoop (p_var, e1) (i_var, bound) body) = do
  v <- eval e1
  n <- eval bound
  case n of
    ValInt n' -> do
      localEnv (envExtend p_var v . envExtend i_var (ValInt 0)) $
        forLoop p_var i_var n' body
    _ -> failure "For loop: non-integer end"

eval (WhileLoop (p_var, e1) cond body) = do
  p <- eval e1
  let loop p = do
        cond' <- localEnv (envExtend p_var p) $ eval cond
        case cond' of
          ValBool True -> do
            p' <- localEnv (envExtend p_var p) $ eval body
            loop p'  -- Continue with the updated value of p
          ValBool False -> pure p
          _ -> failure "While loop: non-boolean condition"
  loop p

