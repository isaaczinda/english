{-|
Module       : SimpleExpr
Description  : An AST and parser for simple arithmetic expressions with variables
Maintainer   : CS 131, Programming Languages (Melissa O'Neill, Chris Stone, Ben Wiedermann)
-}

module SimpleExpr where

import ParserCombinators  -- so we can define our own parser for expressions


--------------------------------------------------------------------------------
-- Abstract syntax 
--
-- Normally, we'd keep the abstract syntax in a separate module. But we're
-- combining the AST and the parser into one file, so that you don't have to
-- switch back and forth between files.
--------------------------------------------------------------------------------


-- | Simple arithmetic expressions, with variables
data Expr = Var String            -- ^ a variable
          | Number Integer        -- ^ an integer
          | BinOp Op Expr Expr    -- ^ a binary operation over expressions
  deriving (Show, Eq, Ord)

-- | Valid operations or addition, subtraction, multiplication, and division.  
data Op = Plus | Minus | Times | Divide
  deriving (Show, Eq, Ord)


--------------------------------------------------------------------------------
-- Parser 
--
-- This parser is fully defined. You should look it over to understand it, and
-- play around with some examples.
--
-- NOTE: Because this parser is implemented using parser combinators, the parser
--       won't work until you've completed all the definitions in ParserCombinators.hs.
--------------------------------------------------------------------------------  


-- | An expression is an 'expr', followed by whitespace
expression :: Parser Expr
expression = expr <+-> whitespace


-- | An expr is a 'term' followed by 0 or more occurrences of 'plusminus'
expr :: Parser Expr
expr = chainl1 term plusminus


-- | A term is a 'factor' followed by 0 or more occurrences of 'timesdivide'
term :: Parser Expr
term = chainl1 factor timesdivide


-- | A factor is a 'num' or an 'ident' or a parenthetical expression
factor :: Parser Expr
factor =          (num                          >>=: \n  -> Number n)
              <|> (ident                        >>=: \id -> Var id)
              <|> (parens expr)


-- | A plusminus is either the symbol '+' or the symbol '-'
--   Note that this is a parsing function that itself produces a function. The
--   resulting function takes two 'Expr's as input and combines them with the
--   appropriate 'BinOp'.
plusminus :: Parser (Expr -> Expr -> Expr)
plusminus =       (text "+"                     >>:  BinOp Plus)
              <|> (text "-"                     >>:  BinOp Minus)


-- | A timesdivide is either the symbol '*' or the symbol '/'
--   Note that this is a parsing function that itself produces a function. The
--   resulting function takes two 'Expr's as input and combines them with the
--   appropriate 'BinOp'.
timesdivide :: Parser (Expr -> Expr -> Expr)
timesdivide =     (text "*"                     >>:  BinOp Times)
              <|> (text "/"                     >>:  BinOp Divide)

