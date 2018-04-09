{-
  Simple parser for arithmetic expressions built using Parsec
  Pedro Vasconcelos, 2018
-}
module ParsecExample where

import Text.Parsec 
import Data.Char (isDigit)

-- | specialize the generic Parsec parser type for a string parser
type Parser a = Parsec String () a

-- | accept trailing whitespace after some parser
lexeme :: Parser a -> Parser a
lexeme p = do v <- p; spaces; return v

-- | parser an operator or parenthesis 
symbol :: String -> Parser String
symbol s = lexeme (string s) <?> s

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
integer = lexeme (do 
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
topExpr = do spaces; v <- expr; eof; return v

-- | read-eval-print loop for the calculator
readEvalLoop :: IO ()
readEvalLoop = do
  putStr "> "
  s <- getLine
  case parse topExpr "stdin" s of
    Left err -> print err
    Right val -> putStrLn ("= "++ show val)
  readEvalLoop
