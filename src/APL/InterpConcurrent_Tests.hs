module APL.InterpConcurrent_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpConcurrent (runEval)
import APL.Monad
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

evalTest :: String -> Exp -> Val -> TestTree
evalTest desc e v = testCase desc $ do
  res <- runEval $ eval e
  res @?= Right v

evalTestFail :: String -> Exp -> TestTree
evalTestFail desc e = testCase desc $ do
  res <- runEval $ eval e
  case res of
    Left _ -> pure ()
    Right v ->
      assertFailure $
        "Expected error but received this value:\n" ++ show v

tests :: TestTree
tests =
  testGroup
    "Concurrent interpreter"
    [ testGroup
        "Basic operations"
        [ evalTest "Simple arithmetic"
            (Add (CstInt 2) (CstInt 3))
            (ValInt 5)
        ],
      testGroup
        "BothOf operations"
        [ evalTest "Basic both"
            (BothOf (CstInt 1) (CstInt 2))
            (ValTuple [ValInt 1, ValInt 2]),
          evalTest "Nested both"
            (BothOf
              (Add (CstInt 1) (CstInt 2))
              (Mul (CstInt 3) (CstInt 4)))
            (ValTuple [ValInt 3, ValInt 12]),
          evalTestFail "Error propagation"
            (BothOf
              (Div (CstInt 1) (CstInt 0))
              (CstInt 2)),
          evalTest "Complex both with loops"
            (BothOf
              (ForLoop ("x", CstInt 1) ("i", CstInt 5) (Add (Var "x") (CstInt 1)))
              (ForLoop ("y", CstInt 2) ("i", CstInt 3) (Add (Var "y") (CstInt 1))))
            (ValTuple [ValInt 6, ValInt 5])
        ],
      testGroup
        "OneOf operations"
        [ evalTest "For loop the fastest returns first"
            (OneOf
              (ForLoop ("x", CstInt 2) ("i", CstInt 100) (Var "x"))
              (ForLoop ("x", CstInt 1) ("i", CstInt 2) (Var "x")))
            (ValInt 2),
          evalTest "Second succeeds when first fails"
            (OneOf
              (Div (CstInt 1) (CstInt 0))
              (CstInt 2))
            (ValInt 2),
          evalTestFail "Both fail"
            (OneOf
              (Div (CstInt 1) (CstInt 0))
              (Div (CstInt 2) (CstInt 0))),
          evalTest "Infinite loop with alternative"
            (OneOf 
              (WhileLoop ("x", CstInt 2) (CstBool True) (Var "x"))
              (CstInt 2))
            (ValInt 2)
        ],
      testGroup
        "Key-Value operations"
        [ evalTest "Put and Get"
            (Let "x" 
              (KvPut (CstInt 1) (CstInt 42))
              (KvGet (CstInt 1)))
            (ValInt 42),
          evalTest "Get waits for Put"
            (Let "result"
              (BothOf
                (KvGet (CstInt 42))
                (KvPut (CstInt 42) (CstInt 123)))
              (Project (Var "result") 0))
            (ValInt 123),
          evalTest "Multiple concurrent gets"
            (Let "result"
              (BothOf
                (BothOf
                  (KvGet (CstInt 99))
                  (KvGet (CstInt 99)))
                (KvPut (CstInt 99) (CstInt 456)))
              (Project (Var "result") 0))
            (ValTuple [ValInt 456, ValInt 456])
        ],
      testGroup
        "Complex concurrent operations"
        [ evalTest "Nested both with KV operations"
            (BothOf
              (Let "x" 
                (KvPut (CstInt 1) (CstInt 42))
                (KvGet (CstInt 1)))
              (Let "y"
                (KvPut (CstInt 2) (CstInt 84))
                (KvGet (CstInt 2))))
            (ValTuple [ValInt 42, ValInt 84]),
          evalTest "OneOf with KV operations"
            (OneOf
              (KvGet (CstInt 999))  -- This will block
              (Let "x"
                (KvPut (CstInt 888) (CstInt 42))
                (KvGet (CstInt 888))))  -- This should succeed
            (ValInt 42)
        ]
    ]