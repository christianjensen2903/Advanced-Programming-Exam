module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

keywords :: [String]
keywords =
  [ "if",
    "then",
    "else",
    "true",
    "false",
    "let",
    "in",
    "put",
    "get",
    "loop",
    "for",
    "while",
    "do"
  ]

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

lInteger :: Parser Integer
lInteger =
  lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlphaNum)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

pBool :: Parser Bool
pBool =
  choice
    [ const True <$> lKeyword "true",
      const False <$> lKeyword "false"
    ]

pExpList :: Parser Exp
pExpList = do
  x <- pExp
  rest <- chain [x]
  return $ case rest of
    [single] -> single      -- If only one element, return it as an expression
    multiple -> Tuple multiple -- If multiple elements, wrap them in a Tuple
  where
    chain xs =
      choice
        [ do
            lString ","
            y <- pExp
            chain (xs ++ [y]),
          pure xs
        ]

pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> pBool,
      Var <$> lVName,
      try $ lString "(" *> pExpList <* lString ")",
      KvPut <$> (lKeyword "put" *> pAtom) <*> pAtom,
      KvGet <$> (lKeyword "get" *> pAtom)
    ]

pAtomSuffix :: Parser Exp
pAtomSuffix = pAtom >>= \base ->
  choice
    [ Project base <$> (lString "." *> lInteger),
      pure base
    ]

pFExp :: Parser Exp
pFExp = chain =<< pAtomSuffix
  where
    chain x =
      choice
        [ do
            y <- pAtomSuffix
            chain $ Apply x y,
          pure x
        ]

pLoopExp :: Parser Exp
pLoopExp = do
  lKeyword "loop"
  var <- lVName
  lString "="
  initExp <- pExp
  choice
    [ do
        lKeyword "for"
        endVar <- lVName
        lString "<"
        endExp <- pExp
        lKeyword "do"
        ForLoop (var, initExp) (endVar, endExp) <$> pExp,
      do
        lKeyword "while"
        condition <- pExp
        lKeyword "do"
        WhileLoop (var, initExp) condition <$> pExp
    ]

pLExp :: Parser Exp
pLExp =
  choice
    [ If
        <$> (lKeyword "if" *> pExp)
        <*> (lKeyword "then" *> pExp)
        <*> (lKeyword "else" *> pExp),
      Lambda
        <$> (lString "\\" *> lVName)
        <*> (lString "->" *> pExp),
      Let
        <$> (lKeyword "let" *> lVName)
        <*> (lString "=" *> pExp)
        <*> (lKeyword "in" *> pExp),
      pLoopExp,
      pFExp
    ]

pExp5 :: Parser Exp
pExp5 = pLExp >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pLExp
            chain $ Mul x y,
          do
            lString "/"
            y <- pLExp
            chain $ Div x y,
          pure x
        ]

pExp4 :: Parser Exp
pExp4 = pExp5 >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pExp5
            chain $ Add x y,
          do
            lString "-"
            y <- pExp5
            chain $ Sub x y,
          pure x
        ]

pExp3 :: Parser Exp
pExp3 = pExp4 >>= chain
  where
    chain x =
      choice
        [ do
            lString "=="
            y <- pExp4
            chain $ Eql x y,
          pure x
        ]

pExp2 :: Parser Exp
pExp2 = pExp3 >>= chain
  where
    chain x =
      choice
        [ do
            lString "&&"
            y <- pExp3
            chain $ BothOf x y,
          pure x
        ]

pExp1 :: Parser Exp
pExp1 = pExp2 >>= chain
  where
    chain x =
      choice
        [ do
            lString "||"
            y <- pExp2
            chain $ OneOf x y,
          pure x
        ]

pExp :: Parser Exp
pExp = pExp1

parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
