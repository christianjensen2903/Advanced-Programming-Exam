module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

-- Most of these tests are taken from Assignment 3.
tests :: TestTree
tests =
  testGroup
    "Parsing"
    [ -- Example tests
      parserTest "x+y" $ Add (Var "x") (Var "y"),
      parserTestFail "x+",
      testGroup
        "Constants"
        [ parserTest "123" $ CstInt 123,
          parserTest " 123" $ CstInt 123,
          parserTest "123 " $ CstInt 123,
          parserTestFail "123f",
          parserTest "true" $ CstBool True,
          parserTest "false" $ CstBool False
        ],
      testGroup
        "Basic operators"
        [ parserTest "x+y" $ Add (Var "x") (Var "y"),
          parserTest "x-y" $ Sub (Var "x") (Var "y"),
          parserTest "x*y" $ Mul (Var "x") (Var "y"),
          parserTest "x/y" $ Div (Var "x") (Var "y"),
          parserTest "x==y" $ Eql (Var "x") (Var "y")
        ],
      testGroup
        "Operator priority"
        [ parserTest "x+y+z" $ Add (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y-z" $ Sub (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y*z" $ Add (Var "x") (Mul (Var "y") (Var "z")),
          parserTest "x*y*z" $ Mul (Mul (Var "x") (Var "y")) (Var "z"),
          parserTest "x/y/z" $ Div (Div (Var "x") (Var "y")) (Var "z"),
          parserTest "x==y==z" $ Eql (Eql (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y==y+x" $ Eql (Add (Var "x") (Var "y")) (Add (Var "y") (Var "x"))
        ],
      testGroup
        "Conditional expressions"
        [ parserTest "if x then y else z" $ If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then y else if x then y else z" $
            If (Var "x") (Var "y") $
              If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then (if x then y else z) else z" $
            If (Var "x") (If (Var "x") (Var "y") (Var "z")) (Var "z"),
          parserTest "1 + if x then y else z" $
            Add (CstInt 1) (If (Var "x") (Var "y") (Var "z"))
        ],
      testGroup
        "Lexing edge cases"
        [ parserTest "2 " $ CstInt 2,
          parserTest " 2" $ CstInt 2
        ],
      testGroup
        "Tuples"
        [ parserTest "(x, y)" $ Tuple [Var "x", Var "y"],
          parserTest "(1, 2)" $ Tuple [CstInt 1, CstInt 2],
          parserTest "(x, y, z)" $ Tuple [Var "x", Var "y", Var "z"],
          parserTest "(true, false)" $ Tuple [CstBool True, CstBool False],
          parserTest "(x + y, z * w)" $ Tuple [Add (Var "x") (Var "y"), Mul (Var "z") (Var "w")],
          parserTest "(x, (y, z))" $ Tuple [Var "x", Tuple [Var "y", Var "z"]],
          parserTestFail "(x, y, z,)",  -- Example of incorrect syntax with extra comma
          parserTestFail "(x, )",        -- Tuple with a missing element
          parserTestFail "(, x)"         -- Tuple with missing first element
        ],
      testGroup
        "Function application"
        [ parserTest "x y z" $ Apply (Apply (Var "x") (Var "y")) (Var "z"),
          parserTest "x(y z)" $ Apply (Var "x") (Apply (Var "y") (Var "z")),
          parserTest "x y * z" $ Mul (Apply (Var "x") (Var "y")) (Var "z"), 
          parserTestFail "x if x then y else z"
        ],
      testGroup
        "get, put"
        [ parserTest "put x y" $ KvPut (Var "x") (Var "y"),
          parserTest "put x(y)" $ KvPut (Var "x") (Var "y"),
          parserTest "put x y - z" $ Sub (KvPut (Var "x") (Var "y")) (Var "z"),
          parserTest "putxy" $ Var "putxy",
          parserTest "get x + y" $ Add (KvGet (Var "x")) (Var "y"),
          parserTest "getx" $ Var "getx",
          parserTest "printx" $ Var "printx"
        ],
      testGroup
        "Let binding"
        [ parserTest "let x = y in z" $ Let "x" (Var "y") (Var "z"),
          parserTest "let x = 1 in x + 1" $ Let "x" (CstInt 1) (Add (Var "x") (CstInt 1)),
          parserTestFail "let true = y in z",
          parserTestFail "x let v = 2 in v"
        ],
      testGroup
        "Lambdas"
        [ parserTest "\\x -> x" $ Lambda "x" (Var "x"),
          parserTest "\\x -> \\y -> x" $ Lambda "x" (Lambda "y" (Var "x")),
          parserTest "let f = \\x -> x in f 1" $ Let "f" (Lambda "x" (Var "x")) (Apply (Var "f") (CstInt 1)),
          parserTestFail "\\x ->"
        ],
        -- Mention. Unsure if x. 1 is valid.
      testGroup
        "Project"
        [ parserTest "x.1" $ Project (Var "x") 1,
          parserTestFail "x.1.2",
          parserTestFail "x.",
          parserTestFail "x.y"
        ],
      testGroup
        "Loops"
        [ parserTest "loop x = 1 for y < 10 do x + y" $ ForLoop ("x", CstInt 1) ("y", CstInt 10) (Add (Var "x") (Var "y")),
          parserTest "loop x = 1 while x == 10 do x + 1" $ WhileLoop ("x", CstInt 1) (Eql (Var "x") (CstInt 10)) (Add (Var "x") (CstInt 1)),
          parserTestFail "loop x = 1 for y < 10 do x + y while x == 10 do x + 1"
        ],
      -- Following tests are generated by ChatGPT with slight modifications
      testGroup
        "Edge Cases"
        [ testGroup
            "Parentheses in Expressions"
            [ parserTest "(x + y) * z" $ Mul (Add (Var "x") (Var "y")) (Var "z"),
              parserTest "x * (y + z)" $ Mul (Var "x") (Add (Var "y") (Var "z")),
              parserTest "(x)" $ Var "x"
            ],
          testGroup
            "Invalid Syntax (Unmatched Parentheses)"
            [ parserTestFail "(x + y",
              parserTestFail "x + y)"
            ],
          testGroup
            "Variable Names Similar to Keywords"
            [ parserTest "thenx" $ Var "thenx",
              parserTest "ifx y" $ Apply (Var "ifx") (Var "y")
            ],
          testGroup
            "Nested Let Bindings"
            [ parserTest "let x = y in let z = w in v" $ Let "x" (Var "y") (Let "z" (Var "w") (Var "v"))
            ],
          testGroup
            "Function Application with Parentheses"
            [ parserTest "f (x + y)" $ Apply (Var "f") (Add (Var "x") (Var "y")),
              parserTest "(f x) y" $ Apply (Apply (Var "f") (Var "x")) (Var "y")
            ],
          testGroup
            "Application of Lambdas"
            [ parserTest "(\\x -> x) 1" $ Apply (Lambda "x" (Var "x")) (CstInt 1)
            ]
        ]
    ]
