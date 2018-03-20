{-
  Simple parser for arithmetic expressions built using Parsec
  Pedro Vasconcelos, 2018
-}
module ParsecExample where

import Text.Parsec hiding (token)
  -- hide conflicting definition;
  -- we'll define token for consuming leading whitespace
import Data.Char (isDigit)

-- | specialize the generic Parsec parser type for a string parser
type Parser a = Parsec String () a

-- | accept leading spaces, newlines etc. before some parser
-- `try' is needed so that the token parser fails
-- if `spaces' consumes input but `p' doesn't
token :: Parser a -> Parser a
token p = try (spaces >> p)

-- | parser an operator or parenthesis symbol
symbol :: String -> Parser String
symbol s = token (string s) <?> s

-- | accept parenthesis around a parser
parens :: Parser a -> Parser a
parens p = do
  symbol "("
  a <- p
  symbol ")"
  return a

-- | expression parser
expr :: Parser Integer
expr    = term   `chainl1` addop
term    = factor `chainl1` mulop
factor  = integer <|>  parens expr

-- | integer literal
integer :: Parser Integer
integer = (token $ do 
  s <- many1 (satisfy isDigit)
  return (read s))
  <?> "integer"

-- | operators
mulop, addop :: Parser (Integer -> Integer -> Integer)
mulop   =   do symbol "*"; return (*)  
            <|> do symbol "/"; return (div) 

addop   =   do symbol "+"; return (+) 
            <|> do symbol "-"; return (-) 


-- | a top-level expression must consume all input
topExpr :: Parser Integer
topExpr = do v <- expr; spaces; eof; return v

-- | read-eval-print loop for the calculator
readEvalLoop :: IO ()
readEvalLoop = do
  putStr "> "
  s <- getLine
  case parse topExpr "stdin" s of
    Left err -> print err
    Right val -> putStrLn ("= "++ show val)
  readEvalLoop
