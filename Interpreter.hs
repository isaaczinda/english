module Interpreter where


import Parser
import ParserBase
import AST
import Data.Map (Map, lookup, empty, insert)


-- start with a dynamic typechecking interpreter

type Env = Map String Value

data Value =
        Closure Literal Env |
        Number Float
    deriving (Show, Eq)

-- unpack a float from a literal, throwing an error if unpack fails
-- use "pattern guards" because we're very fancy
unpackFloat :: Value -> Float
unpackFloat value |  (Number float) <- value = float
                  |  otherwise               = error "expected float."


apply :: Value -> Value -> Value
apply (Closure (FuncLiteral param body defs) env) arg =
        interpexpr body env''
    where
        -- add the value of the argument to the environment
        env' = (insert param arg env)
        -- add the defs (where assignments) to the environment
        env'' = foldl addWhere env' defs

        -- function to fold where assignment
        addWhere env (Assignment name expr) = (insert name val env)
            where val = interpexpr expr env

apply (Number _) _ = error "could not treat float like function."


interpret :: [Statement] -> [Value]
interpret statements = outputs
    where
        start = (Data.Map.empty, []) -- empty map, no outputs yet

        -- used by foldl to combine the results of evaluating statements
        -- together
        combine :: (Env, [Value]) -> Statement -> (Env, [Value])
        combine (env, vals) s = (env', vals')
            where
                (env', newval) = interpstat s env
                vals' = vals ++ newval -- new output after older outputs

        -- run all of the statements
        (_, outputs) = foldl combine start statements

interpstat :: Statement -> Env -> (Env, [Value])
interpstat (Assign (Assignment name expr)) env = (env', [])
    where
        val = interpexpr expr env
        env' =  insert name val env

interpstat (Print expr) env = (env, [val])
    where val = interpexpr expr env


interpexpr :: Expr -> Env -> Value
interpexpr (Apply e1 e2) env =
        apply f arg
    where
        f = interpexpr e1 env
        arg = interpexpr e2 env

interpexpr (If cond main_body else_body) env =
        if run_if then main_body_lit else else_body_lit
    where
        -- true if the if statement condition is true
        cond_res = unpackFloat (interpexpr cond env)
        run_if = cond_res /= 0

        -- evaluate main body and else body
        main_body_lit = interpexpr main_body env
        else_body_lit = interpexpr else_body env

interpexpr (Var name) env =
    case Data.Map.lookup name env of
        Nothing -> error ("undefined variable: " ++ name)
        Just x -> x

-- if we see a func literal, make a closure
interpexpr (Literal f@(FuncLiteral _ _ _)) env = Closure f env

interpexpr (Literal (FloatLiteral float)) env = Number float

interpexpr (Math expr1 Plus expr2) env  =
    Number (unpackFloat (interpexpr expr1 env) + unpackFloat (interpexpr expr2 env))

interpexpr (Math expr1 Minus expr2) env =
    Number (unpackFloat (interpexpr expr1 env) - unpackFloat (interpexpr expr2 env))

interpexpr (Math expr1 Times expr2) env =
    Number (unpackFloat (interpexpr expr1 env) * unpackFloat (interpexpr expr2 env))

interpexpr (Math expr1 Over expr2) env =
    Number (unpackFloat (interpexpr expr1 env) / unpackFloat (interpexpr expr2 env))
