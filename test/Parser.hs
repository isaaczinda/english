import AST
import Parser
import ParserBase
import Test.Hspec

lit :: Float -> Expr
lit f = (Literal (FloatLiteral f))

main = hspec $ do
    describe "Parser" $ do

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



        it ("parses multiplication before adddition") $
            (parse expr "1 plus 2 times 2") `shouldBe` (Math (lit 1) Plus (Math (lit 2) Times (lit 2)))


        -- context "Drawing a line starting at (Lt, (1,4)) w/ attrs [Label \"A\", Dir Rt, Dir Rt, Dir Lt]" $
        --     it ("should end at (Lt, (" ++ show (1+linewid) ++ ",4))") $
        --         snd (doElem (Draw Line [Label "A", Dir Rt, Dir Rt, Dir Lt]) (Lt, (1,4))) `shouldBe` (Lt, (1+linewid,4))

-- *Main> parse program "Let x equal 1."
-- [VarDec "x" (Literal 1.0)]
-- *Main> parse program "Let x equal 1. If x, let x equal 2."
-- [VarDec "x" (Literal 1.0),If (Var "x") (VarDec "x" (Literal 2.0))]
-- *Main> parse program "Let x equal 1 is not zero."
-- [VarDec "x" (Literal 1.0)]
-- *Main> parse program "As long as x, print the value of x."
-- [While (Var "x") (Print (Var "x"))]
-- *Main> parse program "As long as x is not zero, do nothing."
-- [While (Var "x") NoOp]
