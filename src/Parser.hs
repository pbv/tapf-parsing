{-
  Simple library for teaching parser combinators

  NB: for practical use switch to an "industrial" library instead
  e.g Parsec: https://hackage.haskell.org/package/parsec-3.1.13.0

  Pedro Vasconcelos 2018
-}

module Parser where

import Data.Char(isSpace, isAlpha, isAlphaNum, isDigit, ord)
import Control.Monad

-- | type for parser for values of type `a'
newtype Parser a
  = Parser (String -> [(a, String)])

-- | run a parser on a input string
runParser :: Parser a -> String -> [(a, String)]
runParser (Parser f) = f

-- | monad instance for parsers
instance Monad Parser where
   return a =
     Parser (\cs -> [(a, cs)])
   p >>= k  =
     Parser (\cs -> concat [runParser (k a) cs'
                           | (a,cs')<-runParser p cs])

-----------------------------------------------------------
-- Since GHC > 7.8 every instance of Monad must also be
-- an instance of the `Functor` and `Applicative` classes;
-- we provide just "dummy" instances for now
instance Functor Parser where
  fmap = error "fmap: not yet implemented"
  
instance Applicative Parser where
  pure = error "pure: not yet implemented"
  (<*>) = error "<*>: not yet implemented"
-----------------------------------------------------------


--- basic parsers and combinators

-- | empty parser; always fails
empty :: Parser a
empty = Parser (\cs -> [])

-- | combine all answer from two parsers
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> runParser p cs ++ runParser q cs)


-- | deterministic choice
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser (\cs -> take 1 (runParser (p +++ q) cs))

-- | accept a single next character
next :: Parser Char
next = Parser (\cs -> case cs of
                       [] -> []
                       (c:cs') -> [(c, cs')])

-- | accept the end of input
eof :: Parser ()
eof = Parser (\cs -> case cs of
                 [] -> [((), [])]
                 _  -> [])

-- | accept a character satisfying a predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- next;
  if p c then return c
    else empty 

-- | accept a specific character 
char :: Char -> Parser Char
char c = satisfy (==c)

-- | accept a specific string
string :: String -> Parser String
string "" = return ""
string (c:cs) = do
  char c
  string cs
  return (c:cs) 

-- | repeat a parser zero or more times
many :: Parser a -> Parser [a]
many p = many1 p <|> return []

-- | repeat a parser one or more times
many1 :: Parser a -> Parser [a]
many1 p = do { a<-p; as<-many p; return (a:as) }

-- | parse a string of spaces, tabs or newlines
spaces :: Parser String
spaces = many (satisfy isSpace)

-- | parse  using a parser `p`
-- ignoring any initial spaces
token :: Parser a -> Parser a
token p = spaces >> p

-- | parse some symbol e.g. operator or parenthesis
symbol :: String -> Parser String
symbol s = token (string s) 

-- | accept many `p' separated by `sep'
sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = (p `sepBy1` sep) <|> return []

sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` sep = do
  a <- p
  as <- many (sep >> p)
  return (a:as)

-------------------------------------------------------------
-- Example 1: parse rows of comma-separated values (CSV) 
-------------------------------------------------------------

-- | integer literals
integer :: Parser Integer
integer = token $ do
  s <- many1 (satisfy isDigit)
  return (read s)

-- | string literals
stringLit :: Parser String
stringLit = token $ do
  char '\"'
  s <- many (satisfy (/='\"'))
  char '\"'
  return s

type Item = Either Integer String 

item :: Parser Item
item = do n <- integer; return (Left n)
       <|> do s <- stringLit; return (Right s)

-- | parse a single row
row :: Parser [Item]
row = item `sepBy` comma

comma :: Parser Char
comma = token (char ',')

-- | parse many rows terminated by newlines
rows :: Parser [[Item]]
rows = many1 (do r <- row; char '\n'; return r)

------------------------------------------------------
-- Example 2: parse arithmetic expressions
------------------------------------------------------

expr :: Parser Integer
expr  = term `chainl1` addop
  
term = factor `chainl1` mulop       

factor = integer <|>  parens expr

addop = do symbol "+"; return (+)
        <|> do symbol "-"; return (-) 
mulop = do symbol "*"; return (*)
        <|> do symbol "/"; return div 

-- | accept parenthesis around a parser
parens :: Parser a -> Parser a
parens p = do
  symbol "("
  a <- p
  symbol ")"
  return a

-- | chain applications of a parser 
-- using a left-associative operator
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do a <- p; cont a
   where cont a = do f <- op
                     b <- p
                     cont (f a b)
                  <|> return a




