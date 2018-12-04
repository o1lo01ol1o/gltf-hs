module Main (main) where
import Test.Tasty
import Parsing

main = do
  fs <- getGfTLTests
  defaultMain (tests fs)


tests fs = testGroup "Tests" [
  parsingUnitTests fs,
  uriUnitTests fs]
