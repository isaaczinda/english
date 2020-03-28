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


ws = ws_char <+> (many ws_char)
    where ws_char = ((char ' ') <|> (char '\t') <|> (char '\n'))

-- constraint: no parser may match ws before or after chars matched

-- make a parser into one which skips whitespace before running
makeWs :: Parser a -> Parser a
makeWs p = ws <-+> p

optionalWs :: Parser a -> Parser a
optionalWs p = (optional ws) <-+> p


isNotReserved :: String -> Bool
isNotReserved s = (not (elem s reserved))
    where
        reserved = [
            -- funciton reserved words
            "a", "function", "which", "takes", "and", "returns", "as", "it's",
            "result", "where", "equals",
            -- if reserved words
            "if", "otherwise",
            -- apply reserved words
            "applied", "to",
            -- binop reserved words
            "times", "plus", "over", "minus"]


var :: Parser String
var = (letter <:> (many alphanum)) <=> isNotReserved


-- Parses a floting point literal
-- no spaces allowed anywhere
float :: Parser Literal
float =
        ((optional (char '-')) <+> ((digits <++> (string ".") <++> digits) <|> digits)) >>=:
            \(s, x) ->
                case s of
                    (Just _) -> (FloatLiteral ((stringToFloat x) * (-1)))
                    Nothing -> (FloatLiteral (stringToFloat x))
    where
        stringToFloat :: String -> Float
        stringToFloat = read



-- matches whitespace before, not after
function :: Parser Literal
function =
    ((string "a") <+> (strings_ws ["function", "which", "takes"]))
    <-+> (makeWs var)
    <+-> ((strings_ws ["and", "returns"]))
    <+> (makeWs expr)
    <+> (optional (optionalWs where_statement))
    <+-> ((optionalWs (string "as")) <+> (strings_ws ["it's", "result"]))
        >>=: \((v, e), w) -> (FuncLiteral v e (maybe_to_list w))

    where
        maybe_to_list :: Maybe [a] -> [a]
        maybe_to_list x =
            case x of
                (Just x) -> x
                Nothing -> []

        where_statement :: Parser [Assignment]
        where_statement =
                (string "--") <+>
                (optionalWs (string "where")) <-+>
                ((init_dec <++> intermediate_decs <++> final_dec) <|> init_dec)
                <+-> (optionalWs (string "--"))
            where
                init_dec = (makeWs var_dec) >>= \x -> return [x]
                intermediate_decs = many (string "," <-+> (makeWs var_dec))
                final_dec = ((string ",") <+> (string_ws "and") <-+> (makeWs var_dec)) >>= \x -> return [x]

var_dec :: Parser Assignment
var_dec = var <+-> (string_ws "equals") <+> (makeWs expr) >>=:
    \(v,e) -> (Assignment v e)

-- matches whitespace before
literal :: Parser Literal
literal = function <|> float

-- top-level expression
expr :: Parser Expr
expr = if_otherwise <|> add_expr

program :: Parser [Statement]
program = (optional ws) <-+> (statement <:> (many (makeWs statement))) <+-> (optional ws)
    where
        statement :: Parser Statement
        statement = dec <|> println

        dec :: Parser Statement
        dec = (var_dec) <+-> (string ".")
            >>=: \x -> (Assign x)

        println :: Parser Statement
        println =
            (string "print" <+> strings_ws ["the", "value", "of"]) <-+> (makeWs expr) <+-> string "."
                >>=: \e -> (Print e)


if_otherwise :: Parser Expr
if_otherwise =
    add_expr <+-> (string_ws "if") <+> (makeWs add_expr)
    <+-> (string ",") <+-> (string_ws "otherwise") <+> (makeWs add_expr)
        >>=: \((t,c),f) -> (If c t f)

-- takes in 2 expressions, combines them into one expression using the op
make_op_expr op = \e1 -> \e2 -> Math e1 op e2

-- these parsers all produce functions which take two expressions as inputs
-- and produce an expression with the binop as output
times_op = (string_ws "times") <+> ws >>=: \_ -> (make_op_expr Times)
over_op = (string_ws "over") <+> ws >>=: \_ -> (make_op_expr Over)
plus_op = (string_ws "plus") <+> ws >>=: \_ -> (make_op_expr Plus)
minus_op = (string_ws "minus") <+> ws >>=: \_ -> (make_op_expr Minus)
apply_op = (strings_ws ["applied", "to"]) <+> ws >>=: \_ -> Apply

add_expr = chainl1 mult_expr (plus_op <|> minus_op)
mult_expr = chainl1 apply_expr (over_op <|> times_op)
apply_expr = chainl1 prim apply_op
prim = literal_expr <|> var_expr
    where
        var_expr = (var >>=: \v -> (Var v))
        literal_expr = literal >>=: \x -> (Literal x)
