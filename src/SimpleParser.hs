{-
  Simple library for teaching parser combinators

  NB: for real application an Parsec instead of this
  (https://hackage.haskell.org/package/parsec)

  Pedro Vasconcelos 2018
-}

module SimpleParser where

import Data.Char(isDigit)
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
-- an instance of the `Functor` and `Applicative` classes
instance Functor Parser where
  fmap f p = do x<-p; return (f x)

instance Applicative Parser where
  pure = return
  p <*> q = do f<-p; x<-q; return (f x)
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
p <|> q = Parser (\cs -> case runParser p cs of
                           [] -> runParser q cs
                           (x:_) -> [x])

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

-- | parse a string of spaces or tabs
spaces :: Parser String
spaces = many (satisfy (\c -> c==' ' || c=='\t'))

-- | parse  using a parser `p`
-- ignoring trailing spaces
lexeme :: Parser a -> Parser a
lexeme p = do v<-p; spaces; return v

-- | parse some symbol e.g. operator or parenthesis
symbol :: String -> Parser String
symbol s = lexeme (string s)

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
integer = lexeme $ do
  s <- many1 (satisfy isDigit)
  return (read s)

-- | string literals
stringLit :: Parser String
stringLit = lexeme $ do
  char '\"'
  s <- many (satisfy (/='\"'))
  char '\"'
  return s

data Item
  = Number Integer
  | Text String
  deriving Show

item :: Parser Item
item = do n <- integer; return (Number n)
       <|>
       do s <- stringLit; return (Text s)

-- | parse a single row
row :: Parser [Item]
row = item `sepBy` comma

comma :: Parser Char
comma = lexeme (char ',')

-- | parse many rows terminated by newlines
rows :: Parser [[Item]]
rows = many1 (do r<-row; newline; return r)

newline :: Parser Char
newline = char '\n'


------------------------------------------------------
-- Example 2: parse arithmetic expressions
------------------------------------------------------

expr :: Parser Integer
expr  = term `chainl1` addop

term = factor `chainl1` mulop

factor = integer <|>  parens expr

addop = do symbol "+"; return (+)
        <|>
        do symbol "-"; return (-)

mulop = do symbol "*"; return (*)
        <|>
        do symbol "/"; return div

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
