# english

*english* is a functional programming language where programs are described in grammatically correct sentences in the english language. For example, `print the value of 3 plus 39.` prints the 42 to the screen.

## setup

 * Clone this repo
 * Install the [Haskell Platform](https://www.haskell.org/platform/)
 * Install the Hspec Haskell testing framework, using `cabal install --lib hspec`
 * Run `ghc -o english Main.hs` to build the english executable

## usage

Run `./english test.eng` to run the *english* interpreter on the file test.eng. To get an idea of how to write programs in *english*, check out the fact.eng or fib.eng programs.

## specifics

 * *english* supports recursion.
 * variables in *english* use lexical scope.
 * *english* is dynamically typed.
 * order of operations: function application, multiplication / division, addition / subtraction

*english* does not allow the use of parenthesis or any operator like this. However, a similar effect can be achieved by using where statements. The function `\x -> (1+x)*3` can be written `a function which takes x -- where i = 1 plus x -- and returns i times 3.`
