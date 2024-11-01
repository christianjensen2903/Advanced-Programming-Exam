module APL.InterpPure_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpPure (runEval)
import APL.Monad
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

evalTest :: String -> Exp -> Val -> TestTree
evalTest desc e v =
  testCase desc $
    runEval (eval e) @?= Right v

evalTestFail :: String -> Exp -> TestTree
evalTestFail desc e =
  testCase desc $
    case runEval (eval e) of
      Left _ -> pure ()
      Right v ->
        assertFailure $
          "Expected error but received this value:\n" ++ show v

tests :: TestTree
tests =
  testGroup
    "Pure interpreter"
    [ 
      -- evalTestFail
      --   "State (unknown key)"
      --   (KvGet (CstInt 0)),
      --
      -- Should work after task A.
      evalTest
        "(e1,e2)"
        (Tuple [CstInt 1, CstInt 2])
        (ValTuple [ValInt 1, ValInt 2]),
      --
      -- Should work after Task B.
      -- evalTest
      --   "For loop"
      --   (ForLoop ("x", CstInt 1) ("i", CstInt 10) (Mul (Var "x") (CstInt 2)))
      --   (ValInt 1024),
      -- --
      -- -- Should work after task C.
      -- evalTest
      --   "e1 && e2"
      --   (BothOf (CstInt 0) (CstInt 1))
      --   (ValTuple [ValInt 0, ValInt 1]),
      -- --
      -- -- Should work after task C.
      -- evalTest
      --   "e1 || e2"
      --   (OneOf (CstInt 0) (CstInt 1))
      --   (ValInt 0),
      -- --
      -- -- Should work after task C.
      -- evalTest
      --   "e1 || e2 (first fails)"
      --   (OneOf (KvGet (CstInt 0)) (CstInt 1))
      --   (ValInt 1),
      -- General tests
      testGroup
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
            (Tuple [CstInt 1, Div (CstInt 2) (CstInt 0)])
        ]
    ]
