module APL.InterpConcurrent (runEval) where
import APL.Monad
import Data.IORef
import KVDB
import Control.Monad (when)
import SPC
import Data.Maybe (isNothing)

data InterpState = InterpState
  { isKvdb :: KVDB Val Val,
    isSpc :: SPC
  }

runEval :: EvalM a -> IO (Either Error a)
runEval m = do
  kvdb <- startKVDB
  spc <- startSPC
  ref <- newIORef Nothing
  let state = InterpState kvdb spc
  evalConcurrent state ref m
  result <- readIORef ref
  case result of
    Just (Right x) -> return $ Right x
    Just (Left e) -> return $ Left e
    Nothing -> return $ Left "Computation did not complete"

evalConcurrent :: InterpState -> IORef (Maybe (Either Error a)) -> EvalM a -> IO ()
evalConcurrent state ref = go envEmpty
  where
    go _ (Pure x) = writeIORef ref (Just (Right x))
    go _ (Free (ErrorOp e)) = writeIORef ref (Just (Left e))
    go env (Free (ReadOp k)) = go env (k env)
    go env (Free (StepOp m)) = go env m
    
    go env (Free (KvGetOp key k)) = do
      val <- kvGet (isKvdb state) key
      go env (k val)
    
    go env (Free (KvPutOp key val m)) = do
      kvPut (isKvdb state) key val
      go env m
    
    go env (Free (BothOfOp m1 m2 k)) = do
      ref1 <- newIORef Nothing
      ref2 <- newIORef Nothing
      
      let job1 = Job $ evalConcurrent state ref1 m1
      let job2 = Job $ evalConcurrent state ref2 m2
      
      jid1 <- jobAdd (isSpc state) job1
      jid2 <- jobAdd (isSpc state) job2
      
      let waitForBoth = do
            (_, reason) <- jobWaitAny (isSpc state) [jid1, jid2]
            case reason of
              DoneCrashed -> writeIORef ref (Just (Left "Task crashed"))
              _ -> do
                r1 <- readIORef ref1
                r2 <- readIORef ref2
                case (r1, r2) of
                  (Just (Right v1), Just (Right v2)) ->
                    go env (k (ValTuple [v1, v2]))
                  (Just (Left e), _) -> writeIORef ref (Just (Left e))
                  (_, Just (Left e)) -> writeIORef ref (Just (Left e))
                  _ -> waitForBoth  -- Keep waiting if not both done
      
      waitForBoth
    
    go env (Free (OneOfOp m1 m2 k)) = do
      ref1 <- newIORef Nothing
      ref2 <- newIORef Nothing
      
      let job1 = Job $ evalConcurrent state ref1 m1
      let job2 = Job $ evalConcurrent state ref2 m2
      
      jid1 <- jobAdd (isSpc state) job1
      jid2 <- jobAdd (isSpc state) job2
      
      let checkResult ref' otherRef = do
            result <- readIORef ref'
            case result of
              Just (Right v) -> go env (k v)
              Just (Left _) -> do
                otherResult <- readIORef otherRef
                case otherResult of
                  Just (Right v) -> go env (k v)
                  _ -> writeIORef ref' (Just (Left "Both tasks failed"))
              Nothing -> return ()
      
      let waitForOne = do
            (finishedId, reason) <- jobWaitAny (isSpc state) [jid1, jid2]
            case reason of
              DoneCrashed -> do
                if finishedId == jid1
                  then checkResult ref2 ref1
                  else checkResult ref1 ref2
              _ -> do
                if finishedId == jid1
                  then do
                    checkResult ref1 ref2
                    r1 <- readIORef ref1
                    when (isNothing r1) waitForOne
                  else do
                    checkResult ref2 ref1
                    r2 <- readIORef ref2
                    when (isNothing r2) waitForOne
      
      waitForOne
