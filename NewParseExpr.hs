import ParserCombinators

-- | Simple arithmetic expressions, with variables
data Expr = Var String            -- ^ a variable
          | Number Integer        -- ^ an integer
          | BinOp Op Expr Expr    -- ^ a binary operation over expressions
  deriving (Show, Eq, Ord)

-- | Valid operations or addition, subtraction, multiplication, and division.
data Op = Plus | Times
  deriving (Show, Eq, Ord)

expr = makeBinOpParser "*" Times <|>
       makeBinOpParser "+" Plus <|>
       parensexpr <|> varexpr <|> numexpr


parensexpr = text "(" <-+> expr <+-> text ")"
varexpr = identifier >>=: Var
numexpr = num >>=: Number

makeBinOpParser :: String -> Op -> (Parser Expr)
makeBinOpParser symbol op = expr <+-> text symbol <+> expr >>=: \(e1, e2) -> BinOp op e1 e2
