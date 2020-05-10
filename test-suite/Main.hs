-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import Test.Tasty
import Core

main :: IO ()
main = do
    defaultMain allTests

allTests ::   TestTree
allTests = testGroup "Tasty tests" [

<<<<<<< HEAD
        testGroup "List of tests:" [testApolonius, testPascal]
=======
        testGroup "List of tests:" [testParallelogram, testPascal, testNinePoints]
>>>>>>> 007921bd7cb5639da4bc6327b225f73c643fb17a
        --testsPrelude
    ]

