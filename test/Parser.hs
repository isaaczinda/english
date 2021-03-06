import AST
import Parser
import ParserBase
import Test.Hspec
import Control.Exception (evaluate)

lit :: Float -> Expr
lit f = (Literal (FloatLiteral f))

-- Expect the parse to error
-- test name, test string
makeErrorTest :: String -> Parser a -> String -> SpecWith ()
makeErrorTest testname parser input = do
        it testname $
            evaluate (parse parser input) `shouldThrow` anyException

main = hspec $ do
    describe "Parse float" $ do
        -- literals
        it ("parses float without decimal") $
            (parse expr "1") `shouldBe` (lit 1)

        it ("parses float with decimal") $
            (parse expr "1.1") `shouldBe` (lit 1.1)

        it ("parses negative float") $
            (parse expr "-1") `shouldBe` (lit (-1))

    describe "Parse function" $ do
        it ("parses a function") $
            (parse expr "a function which takes x and returns x") `shouldBe` Literal (FuncLiteral "x" (Var "x") [])

        it ("parses a function with a one-variable where statement") $
            (parse expr "a function which takes x -- where x equals 1 -- and returns x") `shouldBe` Literal (FuncLiteral "x" (Var "x") [(Assignment "x" (lit 1))])

        it ("parses a function with a two-variable where statement") $
            (parse expr "a function which takes x -- where x equals 1, and y equals 2 -- and returns x") `shouldBe` Literal (FuncLiteral "x" (Var "x") [(Assignment "x" (lit 1)), (Assignment "y" (lit 2))])

        it ("parses a function with a three-variable where statement") $
            (parse expr "a function which takes x -- where x equals 1, z equals 0, and y equals 2 -- and returns x") `shouldBe` Literal (FuncLiteral "x" (Var "x") [(Assignment "x" (lit 1)), (Assignment "z" (lit 0)), (Assignment "y" (lit 2))])

        it ("parses a function with a four-variable where statement") $
            (parse expr "a function which takes x -- where x equals 1, z equals 0, p equals 0, and y equals 2 -- and returns x") `shouldBe` Literal (FuncLiteral "x" (Var "x") [(Assignment "x" (lit 1)), (Assignment "z" (lit 0)), (Assignment "p" (lit 0)), (Assignment "y" (lit 2))])

        makeErrorTest "spaces before and after a function's \"--\" are required" expr "a function which takes x-- where x equals 1 -- and returns x"

        makeErrorTest "spaces before a function's \",\" are required" expr "a function which takes x -- where x equals 1 , and y equals 2 -- and returns x"


    describe "Parse variables" $ do
        it ("parses alpha variable") $
            (parse expr "var") `shouldBe` (Var "var")

        it ("parses alphanumeric variable") $
            (parse expr "var10") `shouldBe` (Var "var10")

        makeErrorTest "does not parse variable with underscore" expr "var_10"
        makeErrorTest "does not parse reserved word 'if' as a variable" expr "if"
        makeErrorTest "does not parse reserved word 'plus' as a variable" expr "plus"

    describe "Parse binary arithmatic" $ do
        -- binary arithmetic parsers
        it ("parses addition") $
            (parse expr "1 plus 2") `shouldBe` (Math (lit 1) Plus (lit 2))

        it ("parses subtraction") $
            (parse expr "1 minus 2") `shouldBe` (Math (lit 1) Minus (lit 2))

        it ("parses multiplication") $
            (parse expr "1 times 2") `shouldBe` (Math (lit 1) Times (lit 2))

        it ("parses division") $
            (parse expr "1 over 2") `shouldBe` (Math (lit 1) Over (lit 2))

        it ("parses multiplication before adddition") $
            (parse expr "1 plus 2 times 2") `shouldBe` (Math (lit 1) Plus (Math (lit 2) Times (lit 2)))

    describe "Parse if" $ do
        it ("parses if-otherwise statement") $
            (parse expr "1 if 1 does not equal 0, otherwise 2") `shouldBe` (If (lit 1) (lit 1) (lit 2))

        makeErrorTest "spaces before if-otherwise comma are prohibited" expr "1 if 1 does not equal 0 , otherwise 2"

        it ("parses application within an if statement") $
            (parse expr "f applied to g if 1 does not equal 0, otherwise 0") `shouldBe` (If (lit 1) (Apply (Var "f") (Var "g")) (lit 0))

        makeErrorTest "fails to parse nested if-otherwise statements" expr "1 if 1 does not equal 0, otherwise 1 if 1 does not equal 0, otherwise 2"

    describe "Parse application" $ do
        it ("parses application") $
            (parse expr "f applied to 2") `shouldBe` (Apply (Var "f") (lit 2))

        it ("parses application in correct order") $
            (parse expr "f applied to g applied to 2") `shouldBe` (Apply (Apply (Var "f") (Var "g")) (lit 2))

    describe "Parse program" $ do
        it ("parses print statement.") $
            (parse program "print the value of 1.") `shouldBe` [(Print (lit 1))]

        it ("parses assign statement.") $
            (parse program "x equals 1.") `shouldBe` [(Assign (Assignment "x" (lit 1)))]

        it ("parses multiple statements.") $
            (parse program "x equals 1. print the value of x.") `shouldBe` [(Assign (Assignment "x" (lit 1))), (Print (Var "x"))]

        it ("allows spaces at end of program.") $
            (parse program "print the value of 1. ") `shouldBe` [(Print (lit 1))]

        it ("allows spaces at beginning of program.") $
            (parse program "    \nprint the value of 1.") `shouldBe` [(Print (lit 1))]
