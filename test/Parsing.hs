{-# Language ScopedTypeVariables #-}
module Parsing where
import Control.Monad (forM)
import Control.Lens ((^.), (^..), (^?), toListOf)
import System.FilePath (joinPath)
import System.Directory (canonicalizePath)
import Data.Codec.Gltf
       (Uri(..), buffersEltUri, imagesEltUri, parse, resolveUri,
        topLevelBuffers, topLevelImages, TopLevel)
import Test.Tasty
import Path (parseAbsDir, parent, parseAbsFile)
import Test.Tasty.HUnit
import System.Directory.PathWalk (pathWalkAccumulate)
import Data.List

rootDir = "../gltf-hs/glTF-Sample-Models/2.0/"

getGfTLTests :: IO [FilePath]
getGfTLTests = do
  fgtls <-
    pathWalkAccumulate rootDir $ \root _ files ->
      forM files $ \file ->
        if ".gltf" `isSuffixOf` file
          then pure [joinPath [root, file]]
          else pure mempty
  pure $ mconcat fgtls



parsingUnitTests ::  [FilePath] -> TestTree
parsingUnitTests (fs :: [FilePath]) =
  let ts = fmap testCase fs
      tests = (\(t, fp) -> t $ go fp) <$> zip ts fs
  in testGroup "Parsing Unit tests" tests
  where
    go fp = do
      _ <- parse fp
      pure ()

uriUnitTests :: [FilePath] -> TestTree
uriUnitTests fs =
  let ts = fmap testCase fs
      tests = (\(t, fp) -> t $ parse fp >>= go fp) <$> zip ts fs
  in testGroup "Uri resolution Unit tests" tests
  where
    go fp (tl :: TopLevel) = do
      cfp <-canonicalizePath fp
      pthRoot <- parseAbsFile cfp
      bffs <-
        mapM
          (resolveUri (parent pthRoot) . unUri)
          (tl ^.. topLevelBuffers . traverse . buffersEltUri)
      imgs <-
        mapM
          (resolveUri (parent pthRoot) . unUri)
          (tl ^.. topLevelImages . traverse . imagesEltUri)
      pure ()
