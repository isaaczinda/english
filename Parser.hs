module Parser where

import ParserBase
import Data.Char
import AST

(<=>) :: Parser a -> (a -> Bool) -> Parser a
p <=> isOkay =
    p >>= \ret_val ->
        if (isOkay ret_val) then (return ret_val) else pfail

-- And operator
(<+>) :: Parser a -> Parser b -> Parser (a, b)
p <+> q =
    p >>= (\r1 -> (q >>= \r2 -> return (r1, r2)))


(<++>) :: Parser [a] -> Parser [a] -> Parser [a]
p <++> q =
    (p <+> q) >>= \(a,b) -> (return (a ++ b))

-- Cons operator
(<:>) :: Parser a -> Parser [a] -> Parser [a]
p <:> q =
    (p <+> q) >>= \(a,b) -> (return (a:b))

(>>=:) :: Parser a -> (a -> b) -> Parser b
p >>=: f =
    p >>= \x -> (return (f x))

(<+->) :: Parser a -> Parser b -> Parser a
p <+-> q =
    (p <+> q) >>= \(a,_) -> (return a)

(<-+>) :: Parser a -> Parser b -> Parser b
p <-+> q =
    (p <+> q) >>= \(_,b) -> (return b)

-- | Adapts 'foldl' to work on parse results
{-
takes
    1) a parser that outputs a
    2) a parser that outputs a "combiner" function, which takes a, a and outputs a
returns: a parser for the final, combined a
-}
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p <+> many (op <+> p)
               >>=: \(head,oprs) -> foldl (\a (f,rest) -> f a rest) head oprs

optional :: Parser a -> Parser (Maybe a)
optional p = (p >>=: \x -> Just x) <|> (return Nothing)


-- (char c) matches the char c
char c = (get <=> \i -> (toLower i) == c) >>=: toLower

-- match the string exactly!
string :: String -> Parser String
string "" = return ""
string (h:t) =
    (char h) <:> (string t)


string_ws :: String -> Parser String
string_ws = \s -> makeWs (string s)

-- Takes list of string and creates a string parser where unlimited whitespace
-- is allowed between before and after each string.
strings_ws :: [String] -> Parser [String]
strings_ws strings = comb_parser
    where
        -- reverse the input list so that the first elements in original list
        -- are last to be folded in, and and are thus matched first.
        parsers = map string_ws (reverse strings)

        -- make the first parser into a list parser instead of a string parser
        fst_str_parser = (head parsers) >>= \x -> return [x]

        -- fold all the parsers together into an
        -- (flip (<:>)) makes it so <:> takes arguments in this order:
        --     pstringlist, pstring
        comb_parser = foldl (flip (<:>)) fst_str_parser (tail parsers)


digit = get <=> isDigit

letter = get <=> isLetter

-- digits must be at least 1 digit long
digits = digit <:> (many digit)

alphanum = digit <|> letter


ws = many ((char ' ') <|> (char '\t') <|> (char '\n'))

-- make a parser into one which skips whitespace before running
makeWs :: Parser a -> Parser a
makeWs p = ws <-+> p <+-> ws


-- takes in 2 expressions, combines them into one expression using the op
make_op_expr op = \e1 -> \e2 -> Math e1 op e2

-- these parsers all produce functions which take two expressions as inputs
-- and produce an expression with the binop as output
times_op = (string_ws "times") >>=: \_ -> (make_op_expr Times)
over_op = (string_ws "over") >>=: \_ -> (make_op_expr Over)
plus_op = (string_ws "plus") >>=: \_ -> (make_op_expr Plus)
minus_op = (string_ws "minus") >>=: \_ -> (make_op_expr Minus)


var :: Parser String
var = (makeWs (letter <:> (many alphanum)))

-- Parses a floting point literal
float :: Parser Literal
float =
        ((digits <++> (string ".") <++> digits) <|> digits) >>=
        (\x -> (return (FloatLiteral (stringToFloat x))))
    where
        stringToFloat :: String -> Float
        stringToFloat = read
--
-- -- allow any amount of whitespace before / after a statement
-- program :: Parser [Statement]
-- program = many (makeWs statement)
--
-- statement :: Parser Statement
-- statement = var_dec <|> do_nothing <|> print_ln <|> if_then

function :: Parser Literal
function =
    (strings_ws ["a", "function", "which", "takes"])
    <-+> var <+-> (strings_ws ["and", "returns"]) <+> expr
    <+> (optional where_statement) <+-> (strings_ws ["as", "it's", "result"])
        >>=: \((v, e), w) -> (FuncLiteral v e (maybe_to_list w))

    where
        maybe_to_list :: Maybe [a] -> [a]
        maybe_to_list x =
            case x of
                (Just x) -> x
                Nothing -> []

where_statement :: Parser [VarDec]
where_statement = (string_ws "where") <-+> (init_dec <|> (init_dec <++> intermediate_decs <++> final_dec))
    where
        init_dec = var_dec >>= \x -> return [x]
        intermediate_decs = many (string_ws "," <-+> var_dec)
        final_dec = ((strings_ws [",", "and"]) <-+> var_dec) >>= \x -> return [x]


literal :: Parser Literal
literal = function <|> (makeWs float)

var_dec :: Parser VarDec
var_dec = var <+-> (string_ws "equals") <+> expr >>=:
    \(v,e) -> (VarDec v e)

-- do_nothing :: Parser Statement
-- do_nothing = strings_ws ["do", "nothing", "."] >>=: \_ -> NoOp
--
-- print_ln :: Parser Statement
-- print_ln = (strings_ws ["print", "the", "value", "of"]) <-+> expr <+-> (string_ws ".") >>=:
--     \e -> (Print e)

-- top-level expression
expr = if_otherwise <|> apply_expr

-- modify in order to allow more "otherwise" statements
if_otherwise :: Parser Expr
if_otherwise = apply_expr <+-> (string_ws "if") <+> apply_expr <+-> (strings_ws [",", "otherwise"]) <+> apply_expr
        >>=: \((t,c),f) -> (If c t f)

-- matches any number of apply expressions
apply_expr :: Parser Expr
apply_expr = chainl1 add_expr apply_op
    where
        apply_op :: Parser (Expr -> Expr -> Expr)
        apply_op = (strings_ws ["applied", "to"]) >>=: \_ -> Apply


add_expr = chainl1 mult_expr (plus_op <|> minus_op)
mult_expr = chainl1 prim (over_op <|> times_op)
prim = var_expr <|> literal_expr <|> expr
    where
        var_expr = (var >>=: \v -> (Var v))
        literal_expr = literal >>=: \x -> (Literal x)
