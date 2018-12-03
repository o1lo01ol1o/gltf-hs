{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module GltfTypes where

import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
import           System.IO                      ( stderr
                                                , hPutStrLn
                                                )
import qualified Data.ByteString.Lazy.Char8    as BSL
import           System.Environment             ( getArgs )
import           Control.Monad                  ( forM_
                                                , mzero
                                                , join
                                                )
import           Control.Applicative
import           Data.Aeson.AutoType.Alternative
import           Data.Aeson                     ( decode
                                                , Value(..)
                                                , FromJSON(..)
                                                , ToJSON(..)
                                                , pairs
                                                , (.:)
                                                , (.:?)
                                                , (.=)
                                                , object
                                                )
import           Data.Monoid
import           Data.Text                      ( Text )
import           GHC.Generics

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)


data Attributes = Attributes {
    attributesCOLOR0 :: (Maybe (Double:|:[(Maybe Value)])),
    attributesJOINTS0 :: (Maybe (Double:|:[(Maybe Value)])),
    attributesNORMAL :: (Maybe (Double:|:[(Maybe Value)])),
    attributesWEIGHTS0 :: (Maybe (Double:|:[(Maybe Value)])),
    attributesTEXCOORD1 :: (Maybe (Double:|:[(Maybe Value)])),
    attributesTEXCOORD0 :: (Maybe (Double:|:[(Maybe Value)])),
    attributesTANGENT :: (Maybe (Double:|:[(Maybe Value)])),
    attributesPOSITION :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON Attributes where
  parseJSON (Object v) =
    Attributes <$> v .:?? "COLOR_0" <*> v .:?? "JOINTS_0" <*> v .:?? "NORMAL" <*>
    v .:?? "WEIGHTS_0" <*>
    v .:?? "TEXCOORD_1" <*>
    v .:?? "TEXCOORD_0" <*>
    v .:?? "TANGENT" <*>
    v .: "POSITION"
  parseJSON _ = mzero


instance ToJSON Attributes where
  toJSON (Attributes {..}) =
    object
      [ "COLOR_0" .= attributesCOLOR0
      , "JOINTS_0" .= attributesJOINTS0
      , "NORMAL" .= attributesNORMAL
      , "WEIGHTS_0" .= attributesWEIGHTS0
      , "TEXCOORD_1" .= attributesTEXCOORD1
      , "TEXCOORD_0" .= attributesTEXCOORD0
      , "TANGENT" .= attributesTANGENT
      , "POSITION" .= attributesPOSITION
      ]
  toEncoding (Attributes {..}) =
    pairs
      ("COLOR_0" .= attributesCOLOR0 <> "JOINTS_0" .= attributesJOINTS0 <>
       "NORMAL" .=
       attributesNORMAL <>
       "WEIGHTS_0" .=
       attributesWEIGHTS0 <>
       "TEXCOORD_1" .=
       attributesTEXCOORD1 <>
       "TEXCOORD_0" .=
       attributesTEXCOORD0 <>
       "TANGENT" .=
       attributesTANGENT <>
       "POSITION" .=
       attributesPOSITION)


data KHRDracoMeshCompression = KHRDracoMeshCompression {
    kHRDracoMeshCompressionBufferView :: Double,
    kHRDracoMeshCompressionAttributes :: Attributes
  } deriving (Show,Eq,Generic)


instance FromJSON KHRDracoMeshCompression where
  parseJSON (Object v) = KHRDracoMeshCompression <$> v .:   "bufferView" <*> v .:   "attributes"
  parseJSON _          = mzero


instance ToJSON KHRDracoMeshCompression where
  toJSON     (KHRDracoMeshCompression {..}) = object ["bufferView" .= kHRDracoMeshCompressionBufferView, "attributes" .= kHRDracoMeshCompressionAttributes]
  toEncoding (KHRDracoMeshCompression {..}) = pairs  ("bufferView" .= kHRDracoMeshCompressionBufferView<>"attributes" .= kHRDracoMeshCompressionAttributes)


data DiffuseTexture = DiffuseTexture {
    diffuseTextureIndex :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON DiffuseTexture where
  parseJSON (Object v) = DiffuseTexture <$> v .:   "index"
  parseJSON _          = mzero


instance ToJSON DiffuseTexture where
  toJSON     (DiffuseTexture {..}) = object ["index" .= diffuseTextureIndex]
  toEncoding (DiffuseTexture {..}) = pairs  ("index" .= diffuseTextureIndex)


data KHRMaterialsPbrSpecularGlossiness = KHRMaterialsPbrSpecularGlossiness {
    kHRMaterialsPbrSpecularGlossinessGlossinessFactor :: (Maybe (Double:|:[(Maybe Value)])),
    kHRMaterialsPbrSpecularGlossinessSpecularFactor :: (Maybe ([Double])),
    kHRMaterialsPbrSpecularGlossinessDiffuseFactor :: (Maybe ([Double])),
    kHRMaterialsPbrSpecularGlossinessSpecularGlossinessTexture :: (Maybe (DiffuseTexture:|:[(Maybe Value)])),
    kHRMaterialsPbrSpecularGlossinessDiffuseTexture :: (Maybe (DiffuseTexture:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)


instance FromJSON KHRMaterialsPbrSpecularGlossiness where
  parseJSON (Object v) =
    KHRMaterialsPbrSpecularGlossiness <$> v .:?? "glossinessFactor" <*>
    v .:?? "specularFactor" <*>
    v .:?? "diffuseFactor" <*>
    v .:?? "specularGlossinessTexture" <*>
    v .:?? "diffuseTexture"
  parseJSON _ = mzero


instance ToJSON KHRMaterialsPbrSpecularGlossiness where
  toJSON (KHRMaterialsPbrSpecularGlossiness {..}) =
    object
      [ "glossinessFactor" .= kHRMaterialsPbrSpecularGlossinessGlossinessFactor
      , "specularFactor" .= kHRMaterialsPbrSpecularGlossinessSpecularFactor
      , "diffuseFactor" .= kHRMaterialsPbrSpecularGlossinessDiffuseFactor
      , "specularGlossinessTexture" .=
        kHRMaterialsPbrSpecularGlossinessSpecularGlossinessTexture
      , "diffuseTexture" .= kHRMaterialsPbrSpecularGlossinessDiffuseTexture
      ]
  toEncoding (KHRMaterialsPbrSpecularGlossiness {..}) =
    pairs
      ("glossinessFactor" .= kHRMaterialsPbrSpecularGlossinessGlossinessFactor <>
       "specularFactor" .=
       kHRMaterialsPbrSpecularGlossinessSpecularFactor <>
       "diffuseFactor" .=
       kHRMaterialsPbrSpecularGlossinessDiffuseFactor <>
       "specularGlossinessTexture" .=
       kHRMaterialsPbrSpecularGlossinessSpecularGlossinessTexture <>
       "diffuseTexture" .=
       kHRMaterialsPbrSpecularGlossinessDiffuseTexture)


data KHRTextureTransform = KHRTextureTransform {
    kHRTextureTransformRotation :: (Maybe (Double:|:[(Maybe Value)])),
    kHRTextureTransformOffset :: (Maybe ([Double])),
    kHRTextureTransformScale :: (Maybe ([Double]))
  } deriving (Show,Eq,Generic)


instance FromJSON KHRTextureTransform where
  parseJSON (Object v) = KHRTextureTransform <$> v .:?? "rotation" <*> v .:?? "offset" <*> v .:?? "scale"
  parseJSON _          = mzero


instance ToJSON KHRTextureTransform where
  toJSON (KHRTextureTransform {..}) =
    object
      [ "rotation" .= kHRTextureTransformRotation
      , "offset" .= kHRTextureTransformOffset
      , "scale" .= kHRTextureTransformScale
      ]
  toEncoding (KHRTextureTransform {..}) =
    pairs
      ("rotation" .= kHRTextureTransformRotation <> "offset" .=
       kHRTextureTransformOffset <>
       "scale" .=
       kHRTextureTransformScale)


data ImagesElt = ImagesElt {
    imagesEltUri :: Text,
    imagesEltMimeType :: (Maybe (Text:|:[(Maybe Value)])),
    imagesEltName :: (Maybe (Text:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)


instance FromJSON ImagesElt where
  parseJSON (Object v) = ImagesElt <$> v .:   "uri" <*> v .:?? "mimeType" <*> v .:?? "name"
  parseJSON _          = mzero


instance ToJSON ImagesElt where
  toJSON (ImagesElt {..}) =
    object
      [ "uri" .= imagesEltUri
      , "mimeType" .= imagesEltMimeType
      , "name" .= imagesEltName
      ]
  toEncoding (ImagesElt {..}) =
    pairs
      ("uri" .= imagesEltUri <> "mimeType" .= imagesEltMimeType <> "name" .=
       imagesEltName)


data TexturesElt = TexturesElt {
    texturesEltSampler :: (Maybe (Double:|:[(Maybe Value)])),
    texturesEltName :: (Maybe (Text:|:[(Maybe Value)])),
    texturesEltSource :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON TexturesElt where
  parseJSON (Object v) = TexturesElt <$> v .:?? "sampler" <*> v .:?? "name" <*> v .:   "source"
  parseJSON _          = mzero


instance ToJSON TexturesElt where
  toJSON (TexturesElt {..}) =
    object
      [ "sampler" .= texturesEltSampler
      , "name" .= texturesEltName
      , "source" .= texturesEltSource
      ]
  toEncoding (TexturesElt {..}) =
    pairs
      ("sampler" .= texturesEltSampler <> "name" .= texturesEltName <> "source" .=
       texturesEltSource)


data SamplersElt = SamplersElt {
    samplersEltWrapS :: (Maybe (Double:|:[(Maybe Value)])),
    samplersEltInterpolation :: (Maybe (Text:|:[(Maybe Value)])),
    samplersEltMinFilter :: (Maybe (Double:|:[(Maybe Value)])),
    samplersEltWrapT :: (Maybe (Double:|:[(Maybe Value)])),
    samplersEltMagFilter :: (Maybe (Double:|:[(Maybe Value)])),
    samplersEltInput :: (Maybe (Double:|:[(Maybe Value)])),
    samplersEltName :: (Maybe (Text:|:[(Maybe Value)])),
    samplersEltOutput :: (Maybe (Double:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)


instance FromJSON SamplersElt where
  parseJSON (Object v) =
    SamplersElt <$> v .:?? "wrapS" <*> v .:?? "interpolation" <*>
    v .:?? "minFilter" <*>
    v .:?? "wrapT" <*>
    v .:?? "magFilter" <*>
    v .:?? "input" <*>
    v .:?? "name" <*>
    v .:?? "output"
  parseJSON _ = mzero


instance ToJSON SamplersElt where
  toJSON (SamplersElt {..}) =
    object
      [ "wrapS" .= samplersEltWrapS
      , "interpolation" .= samplersEltInterpolation
      , "minFilter" .= samplersEltMinFilter
      , "wrapT" .= samplersEltWrapT
      , "magFilter" .= samplersEltMagFilter
      , "input" .= samplersEltInput
      , "name" .= samplersEltName
      , "output" .= samplersEltOutput
      ]
  toEncoding (SamplersElt {..}) =
    pairs
      ("wrapS" .= samplersEltWrapS <> "interpolation" .=
       samplersEltInterpolation <>
       "minFilter" .=
       samplersEltMinFilter <>
       "wrapT" .=
       samplersEltWrapT <>
       "magFilter" .=
       samplersEltMagFilter <>
       "input" .=
       samplersEltInput <>
       "name" .=
       samplersEltName <>
       "output" .=
       samplersEltOutput)


data Extensions = Extensions
  { extensionsKHRTextureTransform :: (Maybe (KHRTextureTransform :|: [(Maybe Value)]))
  , extensionsKHRDracoMeshCompression :: (Maybe (KHRDracoMeshCompression :|: [(Maybe Value)]))
  , extensionsKHRMaterialsPbrSpecularGlossiness :: (Maybe (KHRMaterialsPbrSpecularGlossiness :|: [(Maybe Value)]))
  } deriving (Show, Eq, Generic)


instance FromJSON Extensions where
  parseJSON (Object v) =
    Extensions <$> v .:?? "KHR_texture_transform" <*>
    v .:?? "KHR_draco_mesh_compression" <*>
    v .:?? "KHR_materials_pbrSpecularGlossiness"
  parseJSON _ = mzero


instance ToJSON Extensions where
  toJSON (Extensions {..}) =
    object
      [ "KHR_texture_transform" .= extensionsKHRTextureTransform
      , "KHR_draco_mesh_compression" .= extensionsKHRDracoMeshCompression
      , "KHR_materials_pbrSpecularGlossiness" .=
        extensionsKHRMaterialsPbrSpecularGlossiness
      ]
  toEncoding (Extensions {..}) =
    pairs
      ("KHR_texture_transform" .= extensionsKHRTextureTransform <>
       "KHR_draco_mesh_compression" .=
       extensionsKHRDracoMeshCompression <>
       "KHR_materials_pbrSpecularGlossiness" .=
       extensionsKHRMaterialsPbrSpecularGlossiness)


data TargetsElt = TargetsElt
  { targetsEltNORMAL :: (Maybe (Double :|: [(Maybe Value)]))
  , targetsEltTANGENT :: (Maybe (Double :|: [(Maybe Value)]))
  , targetsEltPOSITION :: Double
  } deriving (Show, Eq, Generic)


instance FromJSON TargetsElt where
  parseJSON (Object v) =
    TargetsElt <$> v .:?? "NORMAL" <*> v .:?? "TANGENT" <*> v .: "POSITION"
  parseJSON _ = mzero


instance ToJSON TargetsElt where
  toJSON     (TargetsElt {..}) = object ["NORMAL" .= targetsEltNORMAL, "TANGENT" .= targetsEltTANGENT, "POSITION" .= targetsEltPOSITION]
  toEncoding (TargetsElt {..}) = pairs  ("NORMAL" .= targetsEltNORMAL<>"TANGENT" .= targetsEltTANGENT<>"POSITION" .= targetsEltPOSITION)


data PrimitivesElt = PrimitivesElt {
    primitivesEltExtensions :: (Maybe (Extensions:|:[(Maybe Value)])),
    primitivesEltMode :: (Maybe (Double:|:[(Maybe Value)])),
    primitivesEltMaterial :: (Maybe (Double:|:[(Maybe Value)])),
    primitivesEltIndices :: (Maybe (Double:|:[(Maybe Value)])),
    primitivesEltAttributes :: Attributes,
    primitivesEltTargets :: (Maybe ([TargetsElt]))
  } deriving (Show,Eq,Generic)


instance FromJSON PrimitivesElt where
  parseJSON (Object v) = PrimitivesElt <$> v .:?? "extensions" <*> v .:?? "mode" <*> v .:?? "material" <*> v .:?? "indices" <*> v .:   "attributes" <*> v .:?? "targets"
  parseJSON _          = mzero


instance ToJSON PrimitivesElt where
  toJSON (PrimitivesElt {..}) =
    object
      [ "extensions" .= primitivesEltExtensions
      , "mode" .= primitivesEltMode
      , "material" .= primitivesEltMaterial
      , "indices" .= primitivesEltIndices
      , "attributes" .= primitivesEltAttributes
      , "targets" .= primitivesEltTargets
      ]
  toEncoding (PrimitivesElt {..}) =
    pairs
      ("extensions" .= primitivesEltExtensions <> "mode" .= primitivesEltMode <>
       "material" .=
       primitivesEltMaterial <>
       "indices" .=
       primitivesEltIndices <>
       "attributes" .=
       primitivesEltAttributes <>
       "targets" .=
       primitivesEltTargets)


data MeshesElt = MeshesElt {
    meshesEltName :: (Maybe (Text:|:[(Maybe Value)])),
    meshesEltPrimitives :: [PrimitivesElt],
    meshesEltWeights :: (Maybe ([Double]))
  } deriving (Show,Eq,Generic)


instance FromJSON MeshesElt where
  parseJSON (Object v) = MeshesElt <$> v .:?? "name" <*> v .:   "primitives" <*> v .:?? "weights"
  parseJSON _          = mzero


instance ToJSON MeshesElt where
  toJSON (MeshesElt {..}) =
    object
      [ "name" .= meshesEltName
      , "primitives" .= meshesEltPrimitives
      , "weights" .= meshesEltWeights
      ]
  toEncoding (MeshesElt {..}) =
    pairs
      ("name" .= meshesEltName <> "primitives" .= meshesEltPrimitives <>
       "weights" .=
       meshesEltWeights)


data EmissiveTexture = EmissiveTexture {
    emissiveTextureTexCoord :: (Maybe (Double:|:[(Maybe Value)])),
    emissiveTextureIndex :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON EmissiveTexture where
  parseJSON (Object v) = EmissiveTexture <$> v .:?? "texCoord" <*> v .:   "index"
  parseJSON _          = mzero


instance ToJSON EmissiveTexture where
  toJSON     (EmissiveTexture {..}) = object ["texCoord" .= emissiveTextureTexCoord, "index" .= emissiveTextureIndex]
  toEncoding (EmissiveTexture {..}) = pairs  ("texCoord" .= emissiveTextureTexCoord<>"index" .= emissiveTextureIndex)


data BaseColorTexture = BaseColorTexture {
    baseColorTextureExtensions :: (Maybe (Extensions:|:[(Maybe Value)])),
    baseColorTextureTexCoord :: (Maybe (Double:|:[(Maybe Value)])),
    baseColorTextureIndex :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON BaseColorTexture where
  parseJSON (Object v) = BaseColorTexture <$> v .:?? "extensions" <*> v .:?? "texCoord" <*> v .:   "index"
  parseJSON _          = mzero


instance ToJSON BaseColorTexture where
  toJSON (BaseColorTexture {..}) =
    object
      [ "extensions" .= baseColorTextureExtensions
      , "texCoord" .= baseColorTextureTexCoord
      , "index" .= baseColorTextureIndex
      ]
  toEncoding (BaseColorTexture {..}) =
    pairs
      ("extensions" .= baseColorTextureExtensions <> "texCoord" .=
       baseColorTextureTexCoord <>
       "index" .=
       baseColorTextureIndex)


data PbrMetallicRoughness = PbrMetallicRoughness
  { pbrMetallicRoughnessMetallicFactor :: (Maybe (Double :|: [(Maybe Value)]))
  , pbrMetallicRoughnessMetallicRoughnessTexture :: (Maybe (DiffuseTexture :|: [(Maybe Value)]))
  , pbrMetallicRoughnessBaseColorTexture :: (Maybe (BaseColorTexture :|: [(Maybe Value)]))
  , pbrMetallicRoughnessBaseColorFactor :: (Maybe ([Double]))
  , pbrMetallicRoughnessRoughnessFactor :: (Maybe (Double :|: [(Maybe Value)]))
  } deriving (Show, Eq, Generic)


instance FromJSON PbrMetallicRoughness where
  parseJSON (Object v) =
    PbrMetallicRoughness <$> v .:?? "metallicFactor" <*>
    v .:?? "metallicRoughnessTexture" <*>
    v .:?? "baseColorTexture" <*>
    v .:?? "baseColorFactor" <*>
    v .:?? "roughnessFactor"
  parseJSON _ = mzero


instance ToJSON PbrMetallicRoughness where
  toJSON (PbrMetallicRoughness {..}) =
    object
      [ "metallicFactor" .= pbrMetallicRoughnessMetallicFactor
      , "metallicRoughnessTexture" .=
        pbrMetallicRoughnessMetallicRoughnessTexture
      , "baseColorTexture" .= pbrMetallicRoughnessBaseColorTexture
      , "baseColorFactor" .= pbrMetallicRoughnessBaseColorFactor
      , "roughnessFactor" .= pbrMetallicRoughnessRoughnessFactor
      ]
  toEncoding (PbrMetallicRoughness {..}) =
    pairs
      ("metallicFactor" .= pbrMetallicRoughnessMetallicFactor <>
       "metallicRoughnessTexture" .=
       pbrMetallicRoughnessMetallicRoughnessTexture <>
       "baseColorTexture" .=
       pbrMetallicRoughnessBaseColorTexture <>
       "baseColorFactor" .=
       pbrMetallicRoughnessBaseColorFactor <>
       "roughnessFactor" .=
       pbrMetallicRoughnessRoughnessFactor)


data NormalTexture = NormalTexture {
    normalTextureScale :: (Maybe (Double:|:[(Maybe Value)])),
    normalTextureIndex :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON NormalTexture where
  parseJSON (Object v) = NormalTexture <$> v .:?? "scale" <*> v .:   "index"
  parseJSON _          = mzero


instance ToJSON NormalTexture where
  toJSON     (NormalTexture {..}) = object ["scale" .= normalTextureScale, "index" .= normalTextureIndex]
  toEncoding (NormalTexture {..}) = pairs  ("scale" .= normalTextureScale<>"index" .= normalTextureIndex)


data MaterialsElt = MaterialsElt {
    materialsEltEmissiveTexture :: (Maybe (EmissiveTexture:|:[(Maybe Value)])),
    materialsEltOcclusionTexture :: (Maybe (DiffuseTexture:|:[(Maybe Value)])),
    materialsEltExtensions :: (Maybe (Extensions:|:[(Maybe Value)])),
    materialsEltDoubleSided :: (Maybe (Bool:|:[(Maybe Value)])),
    materialsEltPbrMetallicRoughness :: (Maybe (PbrMetallicRoughness:|:[(Maybe Value)])),
    materialsEltEmissiveFactor :: (Maybe ([Double])),
    materialsEltAlphaCutoff :: (Maybe (Double:|:[(Maybe Value)])),
    materialsEltNormalTexture :: (Maybe (NormalTexture:|:[(Maybe Value)])),
    materialsEltName :: (Maybe (Text:|:[(Maybe Value)])),
    materialsEltAlphaMode :: (Maybe (Text:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)


instance FromJSON MaterialsElt where
  parseJSON (Object v) =
    MaterialsElt <$> v .:?? "emissiveTexture" <*> v .:?? "occlusionTexture" <*>
    v .:?? "extensions" <*>
    v .:?? "doubleSided" <*>
    v .:?? "pbrMetallicRoughness" <*>
    v .:?? "emissiveFactor" <*>
    v .:?? "alphaCutoff" <*>
    v .:?? "normalTexture" <*>
    v .:?? "name" <*>
    v .:?? "alphaMode"
  parseJSON _ = mzero


instance ToJSON MaterialsElt where
  toJSON (MaterialsElt {..}) =
    object
      [ "emissiveTexture" .= materialsEltEmissiveTexture
      , "occlusionTexture" .= materialsEltOcclusionTexture
      , "extensions" .= materialsEltExtensions
      , "doubleSided" .= materialsEltDoubleSided
      , "pbrMetallicRoughness" .= materialsEltPbrMetallicRoughness
      , "emissiveFactor" .= materialsEltEmissiveFactor
      , "alphaCutoff" .= materialsEltAlphaCutoff
      , "normalTexture" .= materialsEltNormalTexture
      , "name" .= materialsEltName
      , "alphaMode" .= materialsEltAlphaMode
      ]
  toEncoding (MaterialsElt {..}) =
    pairs
      ("emissiveTexture" .= materialsEltEmissiveTexture <> "occlusionTexture" .=
       materialsEltOcclusionTexture <>
       "extensions" .=
       materialsEltExtensions <>
       "doubleSided" .=
       materialsEltDoubleSided <>
       "pbrMetallicRoughness" .=
       materialsEltPbrMetallicRoughness <>
       "emissiveFactor" .=
       materialsEltEmissiveFactor <>
       "alphaCutoff" .=
       materialsEltAlphaCutoff <>
       "normalTexture" .=
       materialsEltNormalTexture <>
       "name" .=
       materialsEltName <>
       "alphaMode" .=
       materialsEltAlphaMode)


data Extras = Extras {
    extrasAuthor :: Text,
    extrasTitle :: Text,
    extrasLicense :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON Extras where
  parseJSON (Object v) =
    Extras <$> v .: "author" <*> v .: "title" <*> v .: "license"
  parseJSON _ = mzero


instance ToJSON Extras where
  toJSON (Extras {..}) =
    object
      [ "author" .= extrasAuthor
      , "title" .= extrasTitle
      , "license" .= extrasLicense
      ]
  toEncoding (Extras {..}) =
    pairs
      ("author" .= extrasAuthor <> "title" .= extrasTitle <> "license" .=
       extrasLicense)


data Asset = Asset {
    assetCopyright :: (Maybe (Text:|:[(Maybe Value)])),
    assetVersion :: Text,
    assetGenerator :: (Maybe (Text:|:[(Maybe Value)])),
    assetExtras :: (Maybe (Extras:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)


instance FromJSON Asset where
  parseJSON (Object v) = Asset <$> v .:?? "copyright" <*> v .:   "version" <*> v .:?? "generator" <*> v .:?? "extras"
  parseJSON _          = mzero


instance ToJSON Asset where
  toJSON (Asset {..}) =
    object
      [ "copyright" .= assetCopyright
      , "version" .= assetVersion
      , "generator" .= assetGenerator
      , "extras" .= assetExtras
      ]
  toEncoding (Asset {..}) =
    pairs
      ("copyright" .= assetCopyright <> "version" .= assetVersion <> "generator" .=
       assetGenerator <>
       "extras" .=
       assetExtras)


data BuffersElt = BuffersElt {
    buffersEltUri :: Text,
    buffersEltName :: (Maybe (Text:|:[(Maybe Value)])),
    buffersEltByteLength :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON BuffersElt where
  parseJSON (Object v) = BuffersElt <$> v .:   "uri" <*> v .:?? "name" <*> v .:   "byteLength"
  parseJSON _          = mzero


instance ToJSON BuffersElt where
  toJSON (BuffersElt {..}) =
    object
      [ "uri" .= buffersEltUri
      , "name" .= buffersEltName
      , "byteLength" .= buffersEltByteLength
      ]
  toEncoding (BuffersElt {..}) =
    pairs
      ("uri" .= buffersEltUri <> "name" .= buffersEltName <> "byteLength" .=
       buffersEltByteLength)


data SkinsElt = SkinsElt {
    skinsEltJoints :: [Double],
    skinsEltSkeleton :: Double,
    skinsEltName :: Text,
    skinsEltInverseBindMatrices :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON SkinsElt where
  parseJSON (Object v) =
    SkinsElt <$> v .: "joints" <*> v .: "skeleton" <*> v .: "name" <*>
    v .: "inverseBindMatrices"
  parseJSON _ = mzero


instance ToJSON SkinsElt where
  toJSON (SkinsElt {..}) =
    object
      [ "joints" .= skinsEltJoints
      , "skeleton" .= skinsEltSkeleton
      , "name" .= skinsEltName
      , "inverseBindMatrices" .= skinsEltInverseBindMatrices
      ]
  toEncoding (SkinsElt {..}) =
    pairs
      ("joints" .= skinsEltJoints <> "skeleton" .= skinsEltSkeleton <> "name" .=
       skinsEltName <>
       "inverseBindMatrices" .=
       skinsEltInverseBindMatrices)


data Values = Values {
    valuesByteOffset :: Double,
    valuesBufferView :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON Values where
  parseJSON (Object v) = Values <$> v .:   "byteOffset" <*> v .:   "bufferView"
  parseJSON _          = mzero


instance ToJSON Values where
  toJSON     (Values {..}) = object ["byteOffset" .= valuesByteOffset, "bufferView" .= valuesBufferView]
  toEncoding (Values {..}) = pairs  ("byteOffset" .= valuesByteOffset<>"bufferView" .= valuesBufferView)


data Indices = Indices {
    indicesByteOffset :: Double,
    indicesBufferView :: Double,
    indicesComponentType :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON Indices where
  parseJSON (Object v) = Indices <$> v .:   "byteOffset" <*> v .:   "bufferView" <*> v .:   "componentType"
  parseJSON _          = mzero


instance ToJSON Indices where
  toJSON (Indices {..}) =
    object
      [ "byteOffset" .= indicesByteOffset
      , "bufferView" .= indicesBufferView
      , "componentType" .= indicesComponentType
      ]
  toEncoding (Indices {..}) =
    pairs
      ("byteOffset" .= indicesByteOffset <> "bufferView" .= indicesBufferView <>
       "componentType" .=
       indicesComponentType)


data Sparse = Sparse {
    sparseValues :: Values,
    sparseCount :: Double,
    sparseIndices :: Indices
  } deriving (Show,Eq,Generic)


instance FromJSON Sparse where
  parseJSON (Object v) = Sparse <$> v .:   "values" <*> v .:   "count" <*> v .:   "indices"
  parseJSON _          = mzero


instance ToJSON Sparse where
  toJSON     (Sparse {..}) = object ["values" .= sparseValues, "count" .= sparseCount, "indices" .= sparseIndices]
  toEncoding (Sparse {..}) = pairs  ("values" .= sparseValues<>"count" .= sparseCount<>"indices" .= sparseIndices)


data AccessorsElt = AccessorsElt {
    accessorsEltMax :: (Maybe ([Double])),
    accessorsEltByteOffset :: (Maybe (Double:|:[(Maybe Value)])),
    accessorsEltBufferView :: (Maybe (Double:|:[(Maybe Value)])),
    accessorsEltCount :: Double,
    accessorsEltSparse :: (Maybe (Sparse:|:[(Maybe Value)])),
    accessorsEltName :: (Maybe (Text:|:[(Maybe Value)])),
    accessorsEltComponentType :: Double,
    accessorsEltMin :: (Maybe ([Double])),
    accessorsEltType :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON AccessorsElt where
  parseJSON (Object v) =
    AccessorsElt <$> v .:?? "max" <*> v .:?? "byteOffset" <*>
    v .:?? "bufferView" <*>
    v .: "count" <*>
    v .:?? "sparse" <*>
    v .:?? "name" <*>
    v .: "componentType" <*>
    v .:?? "min" <*>
    v .: "type"
  parseJSON _ = mzero


instance ToJSON AccessorsElt where
  toJSON (AccessorsElt {..}) =
    object
      [ "max" .= accessorsEltMax
      , "byteOffset" .= accessorsEltByteOffset
      , "bufferView" .= accessorsEltBufferView
      , "count" .= accessorsEltCount
      , "sparse" .= accessorsEltSparse
      , "name" .= accessorsEltName
      , "componentType" .= accessorsEltComponentType
      , "min" .= accessorsEltMin
      , "type" .= accessorsEltType
      ]
  toEncoding (AccessorsElt {..}) =
    pairs
      ("max" .= accessorsEltMax <> "byteOffset" .= accessorsEltByteOffset <>
       "bufferView" .=
       accessorsEltBufferView <>
       "count" .=
       accessorsEltCount <>
       "sparse" .=
       accessorsEltSparse <>
       "name" .=
       accessorsEltName <>
       "componentType" .=
       accessorsEltComponentType <>
       "min" .=
       accessorsEltMin <>
       "type" .=
       accessorsEltType)


data Orthographic = Orthographic {
    orthographicZfar :: Double,
    orthographicZnear :: Double,
    orthographicXmag :: Double,
    orthographicYmag :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON Orthographic where
  parseJSON (Object v) = Orthographic <$> v .:   "zfar" <*> v .:   "znear" <*> v .:   "xmag" <*> v .:   "ymag"
  parseJSON _          = mzero


instance ToJSON Orthographic where
  toJSON (Orthographic {..}) =
    object
      [ "zfar" .= orthographicZfar
      , "znear" .= orthographicZnear
      , "xmag" .= orthographicXmag
      , "ymag" .= orthographicYmag
      ]
  toEncoding (Orthographic {..}) =
    pairs
      ("zfar" .= orthographicZfar <> "znear" .= orthographicZnear <> "xmag" .=
       orthographicXmag <>
       "ymag" .=
       orthographicYmag)


data Perspective = Perspective {
    perspectiveZfar :: Double,
    perspectiveZnear :: Double,
    perspectiveAspectRatio :: (Maybe (Double:|:[(Maybe Value)])),
    perspectiveYfov :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON Perspective where
  parseJSON (Object v) = Perspective <$> v .:   "zfar" <*> v .:   "znear" <*> v .:?? "aspectRatio" <*> v .:   "yfov"
  parseJSON _          = mzero


instance ToJSON Perspective where
  toJSON (Perspective {..}) =
    object
      [ "zfar" .= perspectiveZfar
      , "znear" .= perspectiveZnear
      , "aspectRatio" .= perspectiveAspectRatio
      , "yfov" .= perspectiveYfov
      ]
  toEncoding (Perspective {..}) =
    pairs
      ("zfar" .= perspectiveZfar <> "znear" .= perspectiveZnear <> "aspectRatio" .=
       perspectiveAspectRatio <>
       "yfov" .=
       perspectiveYfov)


data CamerasElt = CamerasElt
  { camerasEltOrthographic :: (Maybe (Orthographic :|: [(Maybe Value)]))
  , camerasEltPerspective :: (Maybe (Perspective :|: [(Maybe Value)]))
  , camerasEltName :: (Maybe (Text :|: [(Maybe Value)]))
  , camerasEltType :: Text
  } deriving (Show, Eq, Generic)


instance FromJSON CamerasElt where
  parseJSON (Object v) = CamerasElt <$> v .:?? "orthographic" <*> v .:?? "perspective" <*> v .:?? "name" <*> v .:   "type"
  parseJSON _          = mzero


instance ToJSON CamerasElt where
  toJSON (CamerasElt {..}) =
    object
      [ "orthographic" .= camerasEltOrthographic
      , "perspective" .= camerasEltPerspective
      , "name" .= camerasEltName
      , "type" .= camerasEltType
      ]
  toEncoding (CamerasElt {..}) =
    pairs
      ("orthographic" .= camerasEltOrthographic <> "perspective" .=
       camerasEltPerspective <>
       "name" .=
       camerasEltName <>
       "type" .=
       camerasEltType)


data ScenesElt = ScenesElt {
    scenesEltName :: (Maybe (Text:|:[(Maybe Value)])),
    scenesEltNodes :: [Double]
  } deriving (Show,Eq,Generic)


instance FromJSON ScenesElt where
  parseJSON (Object v) = ScenesElt <$> v .:?? "name" <*> v .:   "nodes"
  parseJSON _          = mzero


instance ToJSON ScenesElt where
  toJSON     (ScenesElt {..}) = object ["name" .= scenesEltName, "nodes" .= scenesEltNodes]
  toEncoding (ScenesElt {..}) = pairs  ("name" .= scenesEltName<>"nodes" .= scenesEltNodes)


data Target = Target {
    targetPath :: Text,
    targetNode :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON Target where
  parseJSON (Object v) = Target <$> v .:   "path" <*> v .:   "node"
  parseJSON _          = mzero


instance ToJSON Target where
  toJSON     (Target {..}) = object ["path" .= targetPath, "node" .= targetNode]
  toEncoding (Target {..}) = pairs  ("path" .= targetPath<>"node" .= targetNode)


data ChannelsElt = ChannelsElt {
    channelsEltSampler :: Double,
    channelsEltTarget :: Target
  } deriving (Show,Eq,Generic)


instance FromJSON ChannelsElt where
  parseJSON (Object v) = ChannelsElt <$> v .:   "sampler" <*> v .:   "target"
  parseJSON _          = mzero


instance ToJSON ChannelsElt where
  toJSON     (ChannelsElt {..}) = object ["sampler" .= channelsEltSampler, "target" .= channelsEltTarget]
  toEncoding (ChannelsElt {..}) = pairs  ("sampler" .= channelsEltSampler<>"target" .= channelsEltTarget)


data AnimationsElt = AnimationsElt {
    animationsEltSamplers :: [SamplersElt],
    animationsEltChannels :: [ChannelsElt],
    animationsEltName :: (Maybe (Text:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)


instance FromJSON AnimationsElt where
  parseJSON (Object v) = AnimationsElt <$> v .:   "samplers" <*> v .:   "channels" <*> v .:?? "name"
  parseJSON _          = mzero


instance ToJSON AnimationsElt where
  toJSON     (AnimationsElt {..}) = object ["samplers" .= animationsEltSamplers, "channels" .= animationsEltChannels, "name" .= animationsEltName]
  toEncoding (AnimationsElt {..}) = pairs  ("samplers" .= animationsEltSamplers<>"channels" .= animationsEltChannels<>"name" .= animationsEltName)


data NodesElt = NodesElt {
    nodesEltRotation :: (Maybe ([Double])),
    nodesEltScale :: (Maybe ([Double])),
    nodesEltChildren :: (Maybe ([Double])),
    nodesEltMatrix :: (Maybe ([Double])),
    nodesEltSkin :: (Maybe (Double:|:[(Maybe Value)])),
    nodesEltName :: (Maybe (Text:|:[(Maybe Value)])),
    nodesEltTranslation :: (Maybe ([Double])),
    nodesEltMesh :: (Maybe (Double:|:[(Maybe Value)])),
    nodesEltCamera :: (Maybe (Double:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)


instance FromJSON NodesElt where
  parseJSON (Object v) =
    NodesElt <$> v .:?? "rotation" <*> v .:?? "scale" <*> v .:?? "children" <*>
    v .:?? "matrix" <*>
    v .:?? "skin" <*>
    v .:?? "name" <*>
    v .:?? "translation" <*>
    v .:?? "mesh" <*>
    v .:?? "camera"
  parseJSON _ = mzero


instance ToJSON NodesElt where
  toJSON (NodesElt {..}) =
    object
      [ "rotation" .= nodesEltRotation
      , "scale" .= nodesEltScale
      , "children" .= nodesEltChildren
      , "matrix" .= nodesEltMatrix
      , "skin" .= nodesEltSkin
      , "name" .= nodesEltName
      , "translation" .= nodesEltTranslation
      , "mesh" .= nodesEltMesh
      , "camera" .= nodesEltCamera
      ]
  toEncoding (NodesElt {..}) =
    pairs
      ("rotation" .= nodesEltRotation <> "scale" .= nodesEltScale <> "children" .=
       nodesEltChildren <>
       "matrix" .=
       nodesEltMatrix <>
       "skin" .=
       nodesEltSkin <>
       "name" .=
       nodesEltName <>
       "translation" .=
       nodesEltTranslation <>
       "mesh" .=
       nodesEltMesh <>
       "camera" .=
       nodesEltCamera)


data BufferViewsElt = BufferViewsElt {
    bufferViewsEltByteOffset :: (Maybe (Double:|:[(Maybe Value)])),
    bufferViewsEltName :: (Maybe (Text:|:[(Maybe Value)])),
    bufferViewsEltByteStride :: (Maybe (Double:|:[(Maybe Value)])),
    bufferViewsEltBuffer :: Double,
    bufferViewsEltByteLength :: Double,
    bufferViewsEltTarget :: (Maybe (Double:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)


instance FromJSON BufferViewsElt where
  parseJSON (Object v) = BufferViewsElt <$> v .:?? "byteOffset" <*> v .:?? "name" <*> v .:?? "byteStride" <*> v .:   "buffer" <*> v .:   "byteLength" <*> v .:?? "target"
  parseJON _          = mzero


instance ToJSON BufferViewsElt where
  toJSON (BufferViewsElt {..}) =
    object
      [ "byteOffset" .= bufferViewsEltByteOffset
      , "name" .= bufferViewsEltName
      , "byteStride" .= bufferViewsEltByteStride
      , "buffer" .= bufferViewsEltBuffer
      , "byteLength" .= bufferViewsEltByteLength
      , "target" .= bufferViewsEltTarget
      ]
  toEncoding (BufferViewsElt {..}) =
    pairs
      ("byteOffset" .= bufferViewsEltByteOffset <> "name" .= bufferViewsEltName <>
       "byteStride" .=
       bufferViewsEltByteStride <>
       "buffer" .=
       bufferViewsEltBuffer <>
       "byteLength" .=
       bufferViewsEltByteLength <>
       "target" .=
       bufferViewsEltTarget)


data TopLevel = TopLevel {
    topLevelImages :: (Maybe ([ImagesElt])),
    topLevelTextures :: (Maybe ([TexturesElt])),
    topLevelSamplers :: (Maybe ([SamplersElt])),
    topLevelMeshes :: [MeshesElt],
    topLevelExtensionsUsed :: (Maybe ([Text])),
    topLevelMaterials :: (Maybe ([MaterialsElt])),
    topLevelAsset :: Asset,
    topLevelBuffers :: [BuffersElt],
    topLevelExtensionsRequired :: (Maybe ([Text])),
    topLevelSkins :: (Maybe ([SkinsElt])),
    topLevelAccessors :: [AccessorsElt],
    topLevelCameras :: (Maybe ([CamerasElt])),
    topLevelScenes :: [ScenesElt],
    topLevelAnimations :: (Maybe ([AnimationsElt])),
    topLevelNodes :: [NodesElt],
    topLevelBufferViews :: [BufferViewsElt],
    topLevelScene :: (Maybe (Double:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) =
    TopLevel <$> v .:?? "images" <*> v .:?? "textures" <*> v .:?? "samplers" <*>
    v .: "meshes" <*>
    v .:?? "extensionsUsed" <*>
    v .:?? "materials" <*>
    v .: "asset" <*>
    v .: "buffers" <*>
    v .:?? "extensionsRequired" <*>
    v .:?? "skins" <*>
    v .: "accessors" <*>
    v .:?? "cameras" <*>
    v .: "scenes" <*>
    v .:?? "animations" <*>
    v .: "nodes" <*>
    v .: "bufferViews" <*>
    v .:?? "scene"
  parseJSON _ = mzero


instance ToJSON TopLevel where
  toJSON (TopLevel {..}) =
    object
      [ "images" .= topLevelImages
      , "textures" .= topLevelTextures
      , "samplers" .= topLevelSamplers
      , "meshes" .= topLevelMeshes
      , "extensionsUsed" .= topLevelExtensionsUsed
      , "materials" .= topLevelMaterials
      , "asset" .= topLevelAsset
      , "buffers" .= topLevelBuffers
      , "extensionsRequired" .= topLevelExtensionsRequired
      , "skins" .= topLevelSkins
      , "accessors" .= topLevelAccessors
      , "cameras" .= topLevelCameras
      , "scenes" .= topLevelScenes
      , "animations" .= topLevelAnimations
      , "nodes" .= topLevelNodes
      , "bufferViews" .= topLevelBufferViews
      , "scene" .= topLevelScene
      ]
  toEncoding (TopLevel {..}) =
    pairs
      ("images" .= topLevelImages <> "textures" .= topLevelTextures <>
       "samplers" .=
       topLevelSamplers <>
       "meshes" .=
       topLevelMeshes <>
       "extensionsUsed" .=
       topLevelExtensionsUsed <>
       "materials" .=
       topLevelMaterials <>
       "asset" .=
       topLevelAsset <>
       "buffers" .=
       topLevelBuffers <>
       "extensionsRequired" .=
       topLevelExtensionsRequired <>
       "skins" .=
       topLevelSkins <>
       "accessors" .=
       topLevelAccessors <>
       "cameras" .=
       topLevelCameras <>
       "scenes" .=
       topLevelScenes <>
       "animations" .=
       topLevelAnimations <>
       "nodes" .=
       topLevelNodes <>
       "bufferViews" .=
       topLevelBufferViews <>
       "scene" .=
       topLevelScene)




parse :: FilePath -> IO TopLevel
parse filename = do
        input <- BSL.readFile filename
        case decode input of
                Nothing -> fatal $ case (decode input :: Maybe Value) of
                        Nothing -> "Invalid JSON file: " ++ filename
                        Just v ->
                                "Mismatched JSON value from file: " ++ filename
                Just r -> return (r :: TopLevel)
    where
        fatal :: String -> IO a
        fatal msg = do
                hPutStrLn stderr msg
                exitFailure

main :: IO ()
main = do
        filenames <- getArgs
        forM_
                filenames
                (\f ->
                        parse f
                                >>= (\p ->
                                            p
                                                    `seq` putStrLn
                                                    $     "Successfully parsed "
                                                    ++    f
                                    )
                )
        exitSuccess


