import Parser
import AST
import Data.Map (Map, lookup, empty, insert)


type Env = Map String Float


-- interpret (head (parse program "Let x equal 1.")) Data.Map.empty
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

interpret (Print expr) env = ([print_val], env)
    where print_val = evalexpr expr env


evalexpr :: Expr -> Env -> Float

evalexpr (Literal num) env = num

evalexpr (Var name) env =
    case Data.Map.lookup name env of
        Nothing -> error ("undefined variable: " ++ name)
        Just x -> x

evalexpr (Math expr1 Plus expr2) env  = (evalexpr expr1 env) + (evalexpr expr2 env)
evalexpr (Math expr1 Minus expr2) env = (evalexpr expr1 env) - (evalexpr expr2 env)
evalexpr (Math expr1 Times expr2) env = (evalexpr expr1 env) * (evalexpr expr2 env)
evalexpr (Math expr1 Over expr2) env = (evalexpr expr1 env) / (evalexpr expr2 env)
