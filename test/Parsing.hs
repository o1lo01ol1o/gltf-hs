{-# LANGUAGE ScopedTypeVariables #-}
module Parsing where
import           Control.DeepSeq           (rnf)
import           Control.Lens              ((^..))
import           Control.Monad             (forM)
import           Data.Codec.Gltf           (TopLevel, Uri (..), buffersEltUri,
                                            imagesEltUri, parse, resolveUri,
                                            topLevelBuffers, topLevelImages)
import           Data.List
import           Path                      (parent, parseAbsFile)
import           System.Directory          (canonicalizePath)
import           System.Directory.PathWalk (pathWalkAccumulate)
import           System.FilePath           (joinPath)
import           Test.Tasty
import           Test.Tasty.HUnit

rootDir :: String
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
      _ <- rnf <$> parse fp
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
      bffs <- rnf <$>
        mapM
          (resolveUri (parent pthRoot) . unUri)
          (tl ^.. topLevelBuffers . traverse . buffersEltUri)
      imgs <- rnf <$>
        mapM
          (resolveUri (parent pthRoot) . unUri)
          (tl ^.. topLevelImages . traverse . imagesEltUri)
      pure ()
