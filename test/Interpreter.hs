import Interpreter
import AST
import Test.Hspec
import Control.Exception (evaluate)
import Data.Map

-- interpexpr (parse expr "1 plus 2") Data.Map.empty

num :: Float -> Expr
num f = (Literal (FloatLiteral f))

func :: (String, Expr) -> Expr
func (param,body) = (Literal (FuncLiteral param body []))

interp :: Expr -> Value
interp input = interpexpr input Data.Map.empty

main = hspec $ do
    describe "Evaluates literal" $ do
        it ("evaluates float literal") $
            (interp (num 2)) `shouldBe` (Number 2)
        it ("evaluates function literal") $
            -- makes a function into a closure
            (interp (func ("x",(num 1)))) `shouldBe` (Closure (FuncLiteral "x" (num 1) []) Data.Map.empty)

    describe "Evaluates arithmatic" $ do
        it ("evaluates addition") $
            (interp (Math (num 1) Plus (num 2))) `shouldBe` (Number 3)
        it ("evaluates addition") $
            (interp (Math (num 1) Times (num 2))) `shouldBe` (Number 2)
        it ("evaluates subtraction") $
            (interp (Math (num 1) Minus (num 2))) `shouldBe` (Number (-1))
        it ("evaluates division") $
            (interp (Math (num 1) Over (num 2))) `shouldBe` (Number 0.5)

    describe "Evaluates function application" $ do
        it ("evaluates curried functions") $ do
            -- evaluates x -> y -> (x + y)

            -- setup: here make_adder is a curried function which returns an
            -- add2 type function
            let math = (Math (Var "x") Plus (Var "y"))
            let make_adder = (func ("x", (func ("y", math))))

            -- test to make sure that applying make_adder to 2 generates add 2
            -- function.
            let x_2_env = (insert "x" (Number 2) Data.Map.empty)
            (interp (Apply make_adder (num 2))) `shouldBe` (Closure (FuncLiteral "y" math []) x_2_env)

            -- test to make sure that applying make_adder to 2 then 1 equals 3
            (interp (Apply (Apply make_adder (num 2)) (num 1))) `shouldBe` (Number 3)

        it ("handles variable scope correctly") $ do
            -- evaluates x -> x -> x
            let ret_ident = (func ("x", (func ("x", Var "x"))))
            (interp (Apply (Apply ret_ident (num 2)) (num 1))) `shouldBe` (Number 1)

        it ("handles function with one where definition") $ do
            let where_def =  Assignment "addx" (Literal (FuncLiteral "y" (Math (Var "x") Plus (Var "y")) []))
            let make_adder = (Literal (FuncLiteral "x" (Var "addx") [where_def]))

            (interp (Apply (Apply make_adder (num 2)) (num 1))) `shouldBe` (Number 3)

        it ("handles function with two where definitions") $ do
            let one = Assignment "one" (num 1)
            let add1 = Assignment "add1" (Literal (FuncLiteral "y" (Math (Var "one") Plus (Var "y")) []))
            let return_add1 = (Literal (FuncLiteral "x" (Var "add1") [one, add1]))

            -- ignore the first argument, add 1 to the second argument
            (interp (Apply (Apply return_add1 (num 2)) (num 1))) `shouldBe` (Number 2)

    describe "Evaluates if-otherwise" $ do
        it ("takes the if branch if condition is 1") $
            (interp (If (num 1) (num 1) (num 2))) `shouldBe` (Number 1)

        it ("takes the if branch if condition is -1") $
            (interp (If (num (-1)) (num 1) (num 2))) `shouldBe` (Number 1)

        it ("takes the otherwise branch if condition is 0") $
            (interp (If (num 0) (num 1) (num 2))) `shouldBe` (Number 2)

    describe "Interprets program" $ do
        it ("interprets a print statement") $
            interpret [(Print (num 1))] `shouldBe` [Number 1]

        it ("interprets a series of print statements") $
            interpret [(Print (num 1)), (Print (num 1))] `shouldBe` [Number 1, Number 1]

        it ("interprets assignment followed by print") $
            -- x = 5, print x
            interpret [Assign (Assignment "x" (Literal (FloatLiteral 5.0))), (Print (Var "x"))] `shouldBe` [Number 5]
