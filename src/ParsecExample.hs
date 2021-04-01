{-
  Simple parser for arithmetic expressions built using Parsec
  Pedro Vasconcelos, 2018
-}
module ParsecExample where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Data.Char (isDigit, isAlpha)



-- | specialize the generic Parsec parser type for a string parser
type Parser a = Parsec String () a

data Expr = Var String
          | Number Integer
          | Op (Integer -> Integer -> Integer) Expr Expr
          | Let String Expr Expr

type Env = [(String, Integer)]

eval :: Expr -> Env -> Integer
eval (Number n) env = n
eval (Var x) env = case lookup x env of
              Just v -> v
              Nothing -> error "undefined variable"
eval (Op f e1 e2) env = eval e1 env `f` eval e2 env
eval (Let x e1 e2) env = let v1 = eval e1 env
                          in eval e2 ((x,v1):env)

-- The lexer
lexer       = P.makeTokenParser haskellDef

parens      = P.parens lexer
identifier  = P.identifier lexer
reserved    = P.reserved lexer
integer     = P.integer lexer
symbol      = P.symbol lexer
lexeme      = P.lexeme lexer
reservedOp    = P.reservedOp lexer

-- | expression parser
expr :: Parser Expr
expr    = term `chainl1` addop
term    = factor `chainl1` mulop
factor  = variable <|> let_expr <|> integer_expr <|> parens expr

-- | integer literal
integer_expr :: Parser Expr
integer_expr = lexeme (do
  n <- integer
  return (Number n))
  <?> "number"

variable :: Parser Expr
variable = do x <- identifier; return (Var x)

let_expr :: Parser Expr
let_expr = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return (Let x e1 e2)

-- | operators
mulop, addop :: Parser (Expr -> Expr -> Expr)
mulop   =   do reservedOp "*"; return (Op (*))
            <|>
            do reservedOp "/"; return (Op div)

addop   =   do reservedOp "+"; return (Op (+))
            <|>
            do reservedOp "-"; return (Op (-))


-- | a top-level expression must consume all input
topExpr :: Parser Expr
topExpr = do spaces; v <- expr; eof; return v

-- | read-eval-print loop for the calculator
readEvalLoop :: IO ()
readEvalLoop = do
  putStr "> "
  s <- getLine
  case parse topExpr "stdin" s of
    Left err -> print err
    Right expr -> putStrLn ("= "++ show (eval expr []))
  readEvalLoop
