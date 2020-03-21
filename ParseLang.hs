import ParserBase
import Data.Char

import Data.Map (Map, empty, insert, lookup)


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


-- (char c) matches the char c
char c = (get <=> \i -> (toLower i) == c) >>=: toLower

-- match the string exactly!
string :: String -> Parser String
string "" = return ""
string (h:t) =
    (char h) <:> (string t)


string_ws :: String -> Parser String
string_ws = \s -> makeWs (string s)


-- toBool :: Parser Char
-- toBool = get >>= (\x -> if x == '0' then (return '0') else (return '1'))

digit = get <=> isDigit

letter = get <=> isLetter

-- digits must be at least 1 digit long
digits = digit <:> (many digit)

alphanum = digit <|> letter


ws = many ((char ' ') <|> (char '\t') <|> (char '\n'))

-- make a parser into one which skips whitespace before running
makeWs :: Parser a -> Parser a
makeWs p = ws <-+> p <+-> ws


data BinOp = Plus | Times | Minus | Over
    deriving (Eq, Show)

data Expr =
        Math Expr BinOp Expr |
        Var String |
        Literal Float
    deriving (Eq, Show)

data Statement =
        VarDec String Expr |
        If Expr Statement |
        While Expr Statement |
        Print Expr |
        NoOp
    deriving (Eq, Show)

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
float :: Parser Expr
float =
        ((digits <++> (string ".") <++> digits) <|> digits) >>=
        (\x -> (return (Literal (stringToFloat x))))
    where
        stringToFloat :: String -> Float
        stringToFloat = read

-- allow any amount of whitespace before / after a statement
program :: Parser [Statement]
program = many (makeWs statement)

statement :: Parser Statement
statement = var_dec <|> do_nothing <|> print_ln <|> if_then <|> while

float_ws :: Parser Expr
float_ws = makeWs float

var_dec :: Parser Statement
var_dec = ((string_ws "let") <-+> var <+-> (string_ws "equal") <+> expr <+-> (string_ws ".")) >>=:
    \(v,e) -> (VarDec v e)

do_nothing :: Parser Statement
do_nothing = string_ws "do nothing." >>=: \_ -> NoOp

print_ln :: Parser Statement
print_ln = (string_ws "print the value of") <-+> expr <+-> (string_ws ".") >>=:
    \e -> (Print e)

-- modify in order to allow more "otherwise" statements
if_then :: Parser Statement
if_then = (string_ws "if") <-+> expr <+-> (string_ws ",") <+> statement >>=:
    \(e,s) -> (If e s)

while :: Parser Statement
while = (string_ws "as long as") <-+> expr <+-> (string_ws ",") <+> statement >>=:
    \(e,s) -> (While e s)

-- matches whitespace, then an expresison with binary operators
expr = makeWs ((add_expr <+-> (string_ws "is not zero")) <|> add_expr)
add_expr = chainl1 mult_expr (plus_op <|> minus_op)
mult_expr = chainl1 prim (over_op <|> times_op)
prim = var_expr <|> float_ws <|> expr
    where var_expr = (var >>=: \v -> (Var v))


    -- data Expr =
    --         Math Expr BinOp Expr |
    --         Var String |
    --         Literal Float
    --     deriving (Eq, Show)
    --
    -- data Statement =
    --         VarDec String Expr |
    --         If Expr Statement |
    --         While Expr Statement |
    --         Print Expr |
    --         NoOp
    --     deriving (Eq, Show)

type Env = Map String Float


interpret :: Statement -> Env -> ([Float], Env)

interpret NoOp env = ([], env)

interpret (If cond body) env =
        if run_if then (body_output, body_env) else ([], env)
    where
        -- true if the if statement condition is true
        run_if = (evalexpr cond env) /= 0
        (body_output, body_env) = interpret body env


interpret (VarDec name expr) env =
    ([], (insert name (evalexpr expr env) env))

interpret (While expr body) env =
        if run_while then body_output ++ else ([], env)
    where
        run_while = (evalexpr expr env) /= 0
        (body_output, env') = interpret body env
        
        (rest_output, rest_env) =


evalexpr :: Expr -> Env -> Float
evalexpr = \x -> \x -> 0
