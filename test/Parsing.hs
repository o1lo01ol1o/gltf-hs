{-# Language OverloadedStrings #-}

{-# Language ScopedTypeVariables #-}
module Parsing where
import Control.Monad (filterM, forM_, forM, mapM)

import System.Directory (listDirectory, doesFileExist)
import System.FilePath (takeExtension)
import GltfTypes (parse)
import Test.Tasty
import Test.Tasty.HUnit

import Data.List

getGfTLTests :: IO [FilePath]
getGfTLTests = do
  all <- listDirectory "./test/data/"
  files <- filterM doesFileExist $ map ("./test/data/" <> )all
  let gftls = files -- filter (\ v -> (\ f -> (".gltf" == f)) $ takeExtension v) files -- TODO: rename all the dumb cp artifacts
  pure gftls



parsingUnitTests (fs :: [FilePath]) =
  let ts = fmap testCase fs
      tests = fmap (\(t, fp) -> t $ go fp) $ zip ts fs
  in testGroup "Parsing Unit tests" $ tests
  where
    go fp = do
      parse fp
      pure ()

