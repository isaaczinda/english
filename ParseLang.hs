import ParserBase
import Data.Char


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


-- (char c) matches the char c
char c = get <=> \i -> i == c

-- match the string exactly!
string :: String -> Parser String
string "" = return ""
string (h:t) =
    (char h) <:> (string t)


-- toBool :: Parser Char
-- toBool = get >>= (\x -> if x == '0' then (return '0') else (return '1'))

digit = get <=> isDigit

letter = get <=> isLetter

-- digits must be at least 1 digit long
digits = digit <:> (many digit)

alphanum = digit <|> letter


ws = many (char ' ')

-- make a parser into one which skips whitespace before running
makeWs :: Parser a -> Parser a
makeWs p = (ws <+> p) >>= \(a,b) -> (return b)




data BinOp = Plus | Times | Minus | Over
    deriving (Eq, Show)

data Expr =
        Math Expr BinOp Expr |
        Var String |
        Literal Float
    deriving (Eq, Show)

-- takes in 2 expressions, combines them into one expression using the op
make_op_expr op = \e1 -> \e2 -> Math e1 op e2

-- these parsers all produce functions which take two expressions as inputs
-- and produce an expression with the binop as output
times_op = (makeWs (string "times")) >>=: \_ -> (make_op_expr Times)
over_op = (makeWs (string "over")) >>=: \_ -> (make_op_expr Over)
plus_op = (makeWs (string "plus")) >>=: \_ -> (make_op_expr Plus)
minus_op = (makeWs (string "minus")) >>=: \_ -> (make_op_expr Minus)

var = (makeWs (letter <:> (many alphanum))) >>= \s -> (return (Var s))


-- Parses a floting point literal
float :: Parser Expr
float =
        (makeWs ((digits <++> (string ".") <++> digits) <|> digits)) >>=
        (\x -> (return (Literal (stringToFloat x))))
    where
        stringToFloat :: String -> Float
        stringToFloat = read



expr = add_expr
add_expr = chainl1 mult_expr (plus_op <|> minus_op)
mult_expr = chainl1 prim (over_op <|> times_op)
prim = var <|> float <|> expr
