module Main
  ( main,
  )
where

import Control.Monad (when)
import qualified SuiteOne as S1
import qualified SuiteTwo as S2
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

{-
In our implementation we do not use any kind of short circuiting in our logic to
prematurely end the test if a suite failed before another. We test all properties before
deciding if the test was successful or not. This might not be the behavior we want for
development. Add an argument to the test that enables or disables a fast-fail if a test
suite failed.
-}

main :: IO ()
main = do
  args <- getArgs
  let fastFail = "--fast-fail" `elem` args
  s1success <- S1.runTests
  when (fastFail && not s1success) exitFailure
  s2success <- S2.runTests
  if s2success
    then exitSuccess
    else exitFailure
