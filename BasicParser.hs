{-|
Module       : BasicParser
Description  : The definitions of basic parsers, from class. Also, some
               (partially complete) definitions of parser combinators
Maintainer   : CS 131, Programming Languages (Melissa O'Neill, Chris Stone, Ben Wiedermann)
-}

import Prelude hiding (return)
import Data.Char


-- | A parser is a function that takes an input string and attempts to parse it.
--   Parsers are parameterized by the type of result they give back, when 
--   the parse succeeds.
newtype Parser a = ParsingFunction (String -> Maybe (a, String))


-- | Given a parser and an input string, use the parser to try to match the 
--   input string. If the parse succeeds AND consumes the entire input string,
--   this function returns the result. Otherwise, this function raises an error.
parse :: Parser a -> String -> a
parse (ParsingFunction f) inputString = 
    case f inputString of
        Just (result, "") -> result
        Just (_,    rest) -> error ("After parsing, " ++ show rest 
                                         ++ " remained")
        Nothing           -> error ("No parse at all")


--------------------------------------------------------------------------------
-- Basic parsers 
--
-- You don't need to modify this code for the lab, but you should know what
-- each of these things do.
--------------------------------------------------------------------------------


-- | The parser that always fails.
pfail :: Parser a
pfail = ParsingFunction alwaysFail
    where alwaysFail _ = Nothing


-- | The parser that consumes and returns one character from the input string
get :: Parser Char
get = ParsingFunction readChar
    where readChar (h:t) = Just(h, t)
          readChar _     = Nothing


-- | The parser that ignores the input string and returns a specified value.          
return :: a -> Parser a
return x  = ParsingFunction (\str -> Just (x,str))


--------------------------------------------------------------------------------
-- Parser combinators 
--
-- Parser combinators are "higher-order" parsers: Each combinator takes as input
-- one or more parsers and produces as output a new parser.
--
-- You will complete one of these definitions and add one of your own.
--------------------------------------------------------------------------------


-- | The "Or" combinator: takes two parsers and returns a new parser that 
--   succeeds if either parser succeeds. If neither parser succeeds, the error
--   message will be from the one that got furthest in trying to parse the input.
(<|>) :: Parser a -> Parser a -> Parser a 
(ParsingFunction f) <|> (ParsingFunction g) = ParsingFunction f_or_g
   where f_or_g str =
             case f str of 
                 Nothing -> g str 
                 result  -> result


-- | The "And then" (also known as "Followed by") combinator: takes two parsers.
--   If the first parser fails on the input, then the whole thing fails. If the
--   first parser succeeds on the input, then the second parser runs on the
--   remainder of the input. If the second parser fails, then the whole thing
--   fails. If both parsers succeed, the result is a pair that combines the result
--   from each parser.
--
-- Example:
--
--     *Main> parse (get <+> get) "ab"
--     ('a','b')
(<+>) :: Parser a ->  Parser b -> Parser (a,b)
(ParsingFunction f) <+> (ParsingFunction g) = ParsingFunction f_then_g
    where f_then_g input = 
              case f input of
                  Nothing -> Nothing
                  Just (v, rest)  ->
                      case g rest of 
                          Nothing -> Nothing
                          Just (v', rest') -> Just ((v, v'), rest')


-- | The "Cons" combinator: takes two parsers. The second parser returns a list,
--   each element of which has a type that match the result of the first parser.
--   If the first parser fails on the input, then the whole thing fails. If the
--   first parser succeeds on the input, then the second parser runs on the
--   remainder of the input. If the second parser fails, then the whole thing
--   fails. If both parsers succeed, the result is a list that combines the result
--   from each parser.
--
-- TODO: Finish the implementation of this combinator (see FIXME)
--
-- Example:
--
--      *Main> parse (get <:> return "mused") "a"
--      "amused"
(<:>) :: Parser a ->  Parser [a] -> Parser [a]
(ParsingFunction f) <:> (ParsingFunction g) = ParsingFunction f_cons_g
    where f_cons_g input = 
              case f input of
                  Nothing -> Nothing
                  Just (v, rest)  ->
                      case g rest of 
                          Nothing -> Nothing
                          Just (v', rest') -> Just (v:v', rest')


-- | Given a parser p, many p succeeds if 0 or more instances of p can be applied
--   to the input string. (Similar to * in regular expressions)            
many :: Parser a -> Parser [a]
many p = (p <:> many p)
            <|> return []


-- | Given a parser p, some p succeeds if 1 more or instances of p can be applied
--   to the input string.                              
some :: Parser a -> Parser [a]
some p = p <:> many p


-- | The "Conditional" parser combinator: Takes a parser that gives back a result
--   of type "a", plus a predicate that takes an input of type "a". If the parser
--   fails on the input string, then the whole thing fails. If the parser
--   succeeds AND the predicate is true for the result, then return the result.
--   If the  parser succeeds but the predicate is false for the result, then the
--   whole thing fails.
(<=>) :: Parser a ->  (a -> Bool) -> Parser a
ParsingFunction f <=> cond = ParsingFunction f_if_cond
    where f_if_cond input = 
              case f input of
                  Nothing -> Nothing
                  Just (v, rest)  -> if cond v then Just (v, rest)
                                               else Nothing


-- | Succeeds if the input string matches a single digit.
--   For example:
--      *Main> parse digit "1"
--      '1'
--      *Main> parse digit "a"
--      *** Exception: No parse at all
--      CallStack (from HasCallStack):
--        error, called at BasicParser.hs:12:30 in main:Main
--
--   HINT: Use some combination of the get parser, the <=> parser combinator, and
--         the built-in Haskell function isDigit.
digit :: Parser Char
digit = get <=> isDigit


-- | Succeeds if the input string matches one or more digits.
--
-- For example:
--      *Main> parse digits "012"
--      "012"
--      *Main> parse digits "01a2"
--      "*** Exception: After parsing, "a2" remained
--      CallStack (from HasCallStack):
--        error, called at BasicParser.hs:10:30 in main:Main
--      *Main> parse digits ""
--      "*** Exception: No parse at all
--      CallStack (from HasCallStack):
--        error, called at BasicParser.hs:12:30 in main:Main
--
digits :: Parser String
digits = some digit

