{-|
Module       : ParserCombinators
Description  : Re-definitions of basic parsers and parser combinators from
               class. Also, some  (partially complete) definitions of more 
               parser combinators.               
Maintainer   : CS 131, Programming Languages (Melissa O'Neill, Chris Stone, Ben Wiedermann)
-}

-- The following complicated module declaration makes it so that we can use our
-- parser-combinator library by writing the line:
--
--   import ParserCombinators
--
-- (Without this complicated declaration, we'd have to re-import the basic
-- parsers and parser combinators that are defined elsewhere.)
module ParserCombinators (module ParserCombinators, module ParserBase) where

import Data.Char as Char  -- so we can use toUpper, isDigit, etc. in this file.

--------------------------------------------------------------------------------
-- Basic parsers and parser combinators 
--------------------------------------------------------------------------------

import ParserBase

-- The import above gives us:
--
--    pfail  :: Parser a
--    return :: a -> Parser a
--    get    :: Parser Char
--    <|>    :: Parser a -> Parser a -> Parser a 
--    <||>   :: Parser a -> Parser a -> Parser a 
--    >>=    :: Parser a ->  (a -> Parser b) -> Parser b
--
-- and
--
--    parse  :: String -> Parser a -> a
--
-- The course web page has more information about what these things do, in
-- particular, you can check out
--
--     https://www.cs.hmc.edu/cs131/ParserBaseOperations


--------------------------------------------------------------------------------
-- Useful variations of the general "and then" combinator >>= 
--
-- You don't need to modify these combinators, but you will DEFINITELY want to
-- understand them and practice using them.
--------------------------------------------------------------------------------


-- | Given a parser, transform its result by passing it through a provided function
--
-- TODO: This combinator works, but read it over, make sure you understand it.
--
-- Try an example in ghci, e.g.,
--     parse (get >>=: toUpper) "c"
-- 
infixl 1 >>=:
(>>=:) :: Parser a -> (a -> b) -> Parser b
p >>=: f = p >>= \result -> return (f result)
        -- ADVANCED-HASKELL TIP: In the code above, we could write
        --      \result -> return (f result)
        -- as
        --      return . f
        -- because . is the function composition operator.  Avoiding variables
        -- (such as result) is know as a "point free" style.


-- | Given a parser, ignore is result and instead, if the parser succeeds
--   always return a specific value.
--
-- TODO: This combinator works, but read it over, make sure you understand it.
--
-- Try an example in ghci, e.g.,
--     parse (get >>: 3) "a"
--
infixl 1 >>:
(>>:) :: Parser a -> b -> Parser b
p >>: v = p >>= \_ -> return v


--------------------------------------------------------------------------------
-- Some redefinitions of parser combinators from BasicParser.hs
--
-- These are some of the same combinators defined in BasicParser.hs (i.e., the
-- part of the lab that you just completed). You don't need to modify these
-- definitions. HOWEVER, we've implemented them using various versions of the
-- generalized "and then" combinator >>= . You should compare the implementations
-- in this file to those in  BasicParser.hs, and behold the awesomeness of the
-- >>= combinator.
--------------------------------------------------------------------------------


-- | Given a parser and a predicate, return the result of the parser only if
--   it also satisfies the predicate.
infix 7 <=> 
(<=>) :: Parser a -> (a -> Bool) -> Parser a 
p <=> isOkay = p >>= \r -> if isOkay r then return r else pfail


-- | Given two parsers, p and q, makes a new parser for "p then q" that
--     1. runs the first parser on the input
--     2. runs the second parser on what remains in the input
--     3. returns a pair of their results
--   If either of the parsers fail, the whole thing fails.
infixl 6 <+>
(<+>) :: Parser a -> Parser b -> Parser (a,b)
p <+> q = p >>= \pr ->
              q >>= \qr ->
                  return (pr, qr)
-- Note:  Haskell has syntactic sugar for >>= and >>, so you can instead write:
--        do pr <- p
--           qr <- q
--           return (pr, qr)


-- | Given two parsers, p and q, makes a new parser for "p then q" that
--     1. runs the first parser on the input to generate a thing
--     2. runs the second parser on what remains in the input to make a list of
--        the same kind of thing
--     3. returns a list of things
--   If either of the parsers fail, the whole thing fails.
infixl 6 <:>
(<:>) :: Parser a -> Parser [a] -> Parser [a]
p <:> q = p <+> q >>=: \(a,b) -> a:b


--------------------------------------------------------------------------------
-- Additional useful parsers and parser combinators 
--------------------------------------------------------------------------------


-- | Given two parsers, p and q, makes a new parser for "p then q" that
--     1. runs the first parser on the input
--     2. runs the second parser on what remains in the input
--     3. returns ONLY the results of the first one.
--
-- For example:
--     *ParserCombinators> parse (get <+-> get) "ab"
--     'a'    
infixl 6 <+->
(<+->) :: Parser a -> Parser b -> Parser a
p <+-> q = p <+> q >>= \(a, _) -> return a
-- Alternatively, we could have written:
-- p <+-> q = p <+> q >>=: fst


-- | Given two parsers, p and q, makes a new parser for "p then q" that
--     1. runs the first parser on the input
--     2. runs the second parser on what remains in the input
--     3. returns ONLY the results of the second one.
--
-- For example:
--     *ParserCombinators> parse (get <-+> get) "ab"
--     'b'    
infixl 6 <-+>
(<-+>) :: Parser a -> Parser b -> Parser b
p <-+> q = p <+> q >>=: snd
-- Alternatively, we could have written:
-- p <-+> q = p <+> q >>= \(_, b) -> return b

-- | Given two parsers, p and q, makes a new parser for "p then q" that
--     1. runs the first parser on the input
--     2. runs the second parser on what remains in the input
--     3. ignores both results and returns a Haskell unit, ---[i.e., this]--> ()
--
-- For example:
--     *ParserCombinators> parse (get <-+-> get) "ab"
--     ()
--
-- This implementation works, but you can change it to use >> or >>: instead, if
-- you want.
infixl 6 <-+->
(<-+->) :: Parser a -> Parser b -> Parser ()
p <-+-> q = p <+> q >>= \(_,_) -> return ()


-- | Return a character result only if the character satisfies a given predicate
getCharThat :: (Char -> Bool) -> Parser Char
getCharThat predicate = get <=> predicate
                      <??> "Different kind of character expected"


-- | Parse a single character that is a digit                   
digit :: Parser Char
digit  = getCharThat isDigit


-- | Parse a single alphabetic character (uppercase or lowercase)
letter :: Parser Char
letter = getCharThat isAlpha


-- | Parse a character that is whitespace (e.g., space, tab, newline, etc.)
space :: Parser Char
space = getCharThat isSpace


-- | Parse a character that is either a digit or a letter
alphanum :: Parser Char
alphanum = digit <|> letter


-- | Returns c, if c is the next character in the input
--   NOTE: This parser adds a better error message using <??>. 
--         To see the difference, compare the error messages from these two 
--         parsers that look for a single 'x':
--                parse (get <=> (== 'x')) "y"
--                parse (char 'x') "y"
char :: Char -> Parser Char
char c = getCharThat (==c)
         <??> "Expected '" ++ [c] ++ "'"


-- | Returns a specific sequence of characters, if those are exactly the next
--   characters in the input
string :: String -> Parser String
string ""         = return ""
string str@(h:hs) = char h <:> string hs
                    <??> "Expected '" ++ str ++ "'"


-- | Equivalent to 'some'
--   (because some people like to call the "some" parser "many1")
many1 :: Parser a -> Parser [a]
many1 = some


-- | Like 'many' but instead of returning the results, throws them away.
skipMany :: Parser a -> Parser ()
skipMany p  =     (p <-+-> skipMany p)
              <|> return ()


-- | Like 'many1' but instead of returning the results, throws them away.
skipMany1 :: Parser a -> Parser ()
skipMany1 p = p <-+-> skipMany p


-- | Parse an identifier, defined here as a letter, followed by zero or more
--   alphanumeric characters.
identifier :: Parser String
identifier = letter <:> many alphanum


-- | Parse an Integer
number :: Parser Integer
number = ( (char '-' <:> many digit)  <|> many digit ) >>=: stringToInteger
   where
       stringToInteger :: String -> Integer
       stringToInteger = read
       -- Lab note: The Haskell function 'read' can convert from a string to an Integer
 
       
-- | Parse something that is surrounded by delimiters (e.g., parentheses).
--   Note the order of its arguments: it takes the parser's delimiters *first*,
--   and *then* the thing to parse inside them
between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open <-+> p <+-> close     


-- | A parser that ignores whitespace (by consuming it from the input and
--   returning unit).
whitespace :: Parser ()
whitespace = skipMany space


-- | Returns a parser that parses what p does, but skips any whitespace that
--   appears at the beginning of the input string.
skipws :: Parser a -> Parser a
skipws p = whitespace <-+> p


-- | Like 'number' but skips any whitespace that precedes the number.
num :: Parser Integer
num = skipws number


-- | Like 'identifier' but skips any whitespace that precedes the identifier.
ident :: Parser String
ident = skipws identifier


-- | Like 'char' but skips any whitespace that precedes the char.
sym :: Char -> Parser Char
sym ch = skipws (char ch)


-- | Like 'string' but skips any whitespace that precedes the string.
text :: String -> Parser String
text str = skipws (string str)


-- | A parser for a reserved word. A reserved word has the same restrictions as
--   an identifier, but also has a particular name (e.g., "while")
rword :: String -> Parser String
rword str = ident <=> \ident -> ident == str


-- | Parses the character '('
openparen :: Parser Char
openparen  = sym '('


-- | Parses the character ')'
closeparen :: Parser Char
closeparen = sym ')'


-- | Given a parser p, succeeds if the input string contains a
--   parenthesis-delimited string that matches the parser p.
parens :: Parser a -> Parser a
parens p = between openparen closeparen p


-- | Given two parsers p and sep, succeeds if the input string contains
--   a sep-delimited sequence of one or more things that match p. The delimiters
--   will be thrown away and we'll be left with a list of all the matches for p.        
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = p <+> many (sep <-+> p) >>= \(h,t) -> return (h:t)


-- | Given two parsers p and sep, succeeds if the input string contains
--   a sep-delimited sequence of zero or more things that match p. The delimiters
--   will be thrown away and we'll be left with a (possibly empty) list of all
--   the matches for p. 
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep
             <|> return []


-- | Similar to 'sepBy', but the delimiter must also appear at the end.             
endBy :: Parser a -> Parser b -> Parser [a]
endBy p sep = many (p <+-> sep)


-- | Similar to 'sepBy1', but the delimiter must also appear at the end.             
endBy1 :: Parser a -> Parser b -> Parser [a]
endBy1 p sep = some (p <+-> sep)


--------------------------------------------------------------------------------
-- Chaining 
--
-- You don't need to come up with examples for how to use these combinators.
-- For the next part of the lab, though, you'll want to observe how we use these
-- combinators to parse expressions.
--------------------------------------------------------------------------------


-- | Adapts 'foldr' to work on parse results
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = many (p <+> op) <+> p 
               >>=: \(rops,toe) ->  foldr (\(r,f) a -> f r a) toe rops


-- | Adapts 'foldl' to work on parse results
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p <+> many (op <+> p) 
               >>=: \(head,oprs) -> foldl (\a (f,r) -> f a r) head oprs


--------------------------------------------------------------------------------
-- Better error messages 
-- 
-- These combinators allow us to have nicer error messages when parsing fails.
-- You don't need to modify these combinators.
--------------------------------------------------------------------------------


-- | Given a parser p, makes a new parser where if p fails, the new parser
--   also fails, but unconditionally replaces p's error message with
--   errMsg. All error information from p (including how far it got) is thrown
--   away. (This operator uses <||> which is identical to <|> except that it
--   handles error messages slightly differently).
infixl 3 <??>
(<??>) :: Parser a -> String -> Parser a
parser <??> message = parser <||> fail message


-- | Given a parser p, makes a new parser where if p fails, the new parser
--   also fails, but it can replace a parser's failure error message with
--   a new one.  But unlike <??>, we only do the replacement if the parser got
--   *nowhere* with things. If it made  some headway at all, we let its error
--   message stand, in the hope it'll be more useful.
infixl 3 <???>
(<???>) :: Parser a -> String -> Parser a
parser <???> message = parser <|> fail message

