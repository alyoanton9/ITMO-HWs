module Test(
) where

import Test.HUnit
import ParserImpl

reportMsg :: String -> Bool -> Int -> IO Int
reportMsg msg isProgress count = do
    putStrLn $ "#" ++ show count ++ ": " ++ msg
    return (count+1)

myPutText = PutText reportMsg 0 :: PutText Int

tests = test [  "test_sum1" ~: "1 + 2" ~: 3 ~=? run "1 + 2",
                "test_sum2" ~: "2 + 4 + (-8) +1" ~: -1 ~=?
                                        run "2 + 4 + (-8) +1",
                "test_neg1" ~: "-(2*(-(-3)))" ~: -6 ~=?
                                        run "-(2*(-(-3)))",
                "test_neg2" ~: "-(-8)" ~: 8 ~=? run "-(-8)",
                "test_mul1" ~: "1 + 2*4" ~: 9 ~=? run "1 + 2*4",
                "test_mul2" ~: "2*1*(-1)*(2+3)*(-1)" ~: 10 ~=?
                                        run "2*1*(-1)*(2+3)*(-1)",
                "test_spaces1" ~: "3     -1 \n * 9\t" ~: -6 ~=?
                                        run "3     -1 \n * 9\t",
                "test_spaces2" ~: "1\t\r\n+6\t\t" ~: 7 ~=?
                                        run "1\t\r\n+6\t\t",
                "test_left_assoc1" ~: "-1-1-1" ~: -3 ~=? run "-1-1-1",
                "test_left_assoc2" ~: "2 - (3-1-1)" ~: 1 ~=? run "2 - (3-1-1)",
                "test_mod1" ~: "1 + 3%2" ~: 2 ~=? run "1 + 3%2",
                "test_mod2" ~: "2 * 4 % 3" ~: 2 ~=? run "2 * 4 % 3",
                "test_div1" ~: "19 / 2 / 3" ~: 3 ~=? run "19 / 2 / 3"]

runTests :: IO()
runTests = do
    (testCounts, msgCount) <- runTestText myPutText tests
    putStrLn $ "Message emitted: " ++ show msgCount