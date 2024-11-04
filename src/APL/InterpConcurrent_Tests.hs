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
        "Constants"
        [
          evalTest "Integer" (CstInt 123) (ValInt 123),
          evalTest "Boolean (true)" (CstBool True) (ValBool True),
          evalTest "Boolean (false)" (CstBool False) (ValBool False)
        ],
      testGroup
        "Basic operators"
        [
          evalTest "Addition"
        (Add (CstInt 2) (CstInt 3))
            (ValInt 5),
          evalTest
            "Subtraction"
            (Sub (CstInt 5) (CstInt 3))
            (ValInt 2),
          evalTest
            "Multiplication"
            (Mul (CstInt 2) (CstInt 3))
            (ValInt 6),
        evalTest
          "Division"
          (Div (CstInt 6) (CstInt 3))
          (ValInt 2),
        evalTest
          "Equality"
          (Eql (CstInt 2) (CstInt 2))
          (ValBool True),
        evalTestFail
          "Division by zero"
          (Div (CstInt 7) (CstInt 0))
        ],
      testGroup
        "Conditional expressions"
        [
          evalTest
            "If (true)"
            (If (CstBool True) (CstInt 1) (CstInt 2))
            (ValInt 1),
          evalTest
            "If (false)"
            (If (CstBool False) (CstInt 1) (CstInt 2))
            (ValInt 2)
        ],
      testGroup
        "Let"
        [
          evalTest
            "Let"
            (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
            (ValInt 5),
          evalTest
            "Let (shadowing)"
            (Let "x" (Add (CstInt 2) (CstInt 3))
                    (Let "x" (CstBool True) (Var "x")))
            (ValBool True)
        ],
      testGroup
        "Lambda/apply"
        [
          evalTest
            "Lambda construction"
            (Lambda "x" (Add (Var "x") (CstInt 1)))
            (ValFun [] "x" (Add (Var "x") (CstInt 1))),
          evalTest
            "Apply"
            (Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (CstInt 2))
            (ValInt 3)
        ],
      testGroup
        "Tuple"
        [
          evalTest
            "Tuple construction"
            (Tuple [CstInt 1, CstInt 2])
            (ValTuple [ValInt 1, ValInt 2]),
          evalTestFail
            "Tuple construction (wrong arity)"
            (Tuple [CstInt 1]),
          evalTestFail
            "Error inside tuple"
            (Tuple [CstInt 1, Div (CstInt 2) (CstInt 0)]),
          evalTest
            "Project"
            (Project (Tuple [CstInt 1, CstInt 2]) 1)
            (ValInt 2),
          evalTestFail
            "Project (non-tuple)"
            (Project (CstInt 1) 0),
          evalTestFail
            "Project (index out of bounds)"
            (Project (Tuple [CstInt 1, CstInt 2]) 2),
          evalTest
            "Elements are evaluated left to right"
            (Tuple [Tuple [ 
                KvPut (CstInt 1) (CstBool False),
                KvPut (CstInt 1) (CstBool True),
                KvGet (CstInt 1)
              ], KvGet (CstInt 1)])
            (ValTuple [ValTuple [ValBool False, ValBool True, ValBool True], ValBool True])
        ],
      testGroup
        "Loops"
        [
          testGroup
            "For loop"
            [
              evalTest
                "Works"
                (ForLoop ("x", CstInt 1) ("i", CstInt 10) (Mul (Var "x") (CstInt 2)))
                (ValInt 1024),
              evalTestFail
                "Non-integer end"
                (ForLoop ("x", CstInt 1) ("i", CstBool True) (Mul (Var "x") (CstInt 2))),
              evalTestFail
                "Trying to use variable in bound"
                (ForLoop ("x", CstInt 1) ("i", Var "x") (Mul (Var "x") (CstInt 2))),
              evalTestFail
                "Variable lives after loop"
                (Let "y" (ForLoop ("x", CstInt 1) ("i", CstInt 10) (Mul (Var "x") (CstInt 2))) (Add (Var "y") (Var "x")))
            ],
          testGroup
            "While loop"
            [
              -- Hard to test properly when only equality is supported.
              evalTest
                "Works"
                (WhileLoop ("x", CstInt 1) (Eql (Var "x") (CstInt 1)) (Add (Var "x") (CstInt 1)))
                (ValInt 2),
              evalTest
                "Works 2"
                (WhileLoop ("x", CstInt 1) (Eql (Var "x") (CstInt 5)) (Add (Var "x") (CstInt 1)))
                (ValInt 1),
              evalTestFail
                "Non-boolean condition"
                (WhileLoop ("x", CstInt 1) (CstInt 1) (Mul (Var "x") (CstInt 2))),
              evalTestFail
                "Variable lives after loop"
                (Let "y" (WhileLoop ("x", CstInt 1) (Eql (Var "x") (CstInt 1)) (Add (Var "x") (CstInt 1))) (Add (Var "y") (Var "x")))
            ]
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