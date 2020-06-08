{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
module Data.Codec.Gltf where

import           Control.Applicative
import           Control.DeepSeq            (NFData)
import           Control.Exception.Base     (Exception (..))
import           Control.Lens               ((^.), (^?))
import           Control.Lens.TH            (makeLenses, makePrisms)
import           Control.Monad              (join, mzero)
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Aeson                 (FromJSON (..), ToJSON (..),
                                             Value (..), decode, object, pairs,
                                             (.:), (.:?), (.=))
import           Data.Aeson.Types           (Object, Parser)
import qualified Data.ByteString            as B hiding (stripPrefix)
import qualified Data.ByteString.Lazy.Char8 as BSL hiding (stripPrefix)
import           Data.Foldable              (concat)
import           Data.Int                   (Int64)
import           Data.Maybe                 (isJust)
import           Data.Scientific            (isInteger, toBoundedInteger)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Conversions      (Base64 (..), convertText)
import           GHC.Generics
import           Path                       (Abs, Dir, File, Path, parseRelFile,
                                             toFilePath, (</>))
import           System.Directory           (doesFileExist)
import           System.Exit                (exitFailure)
import qualified Text.URI                   as URI
import           Text.URI.Lens

-- | Workaround for https://github.com/bos/aeson/issues/287.
(.:??) :: FromJSON a =>
            Object
            -> Text -> Parser (Maybe a)
o .:?? val = fmap join (o .:? val)


data Attributes = Attributes {
    _attributesCOLOR0    ::  Maybe Int,
    _attributesJOINTS0   :: Maybe Int,
    _attributesNORMAL    :: Maybe Int,
    _attributesWEIGHTS0  :: Maybe Int,
    _attributesTEXCOORD1 :: Maybe Int,
    _attributesTEXCOORD0 :: Maybe Int,
    _attributesTANGENT   :: Maybe Int,
    _attributesPOSITION  :: Maybe Int
  } deriving (Show,NFData,Ord,Eq,Generic)


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
      [ "COLOR_0" .= _attributesCOLOR0
      , "JOINTS_0" .= _attributesJOINTS0
      , "NORMAL" .= _attributesNORMAL
      , "WEIGHTS_0" .= _attributesWEIGHTS0
      , "TEXCOORD_1" .= _attributesTEXCOORD1
      , "TEXCOORD_0" .= _attributesTEXCOORD0
      , "TANGENT" .= _attributesTANGENT
      , "POSITION" .= _attributesPOSITION
      ]
  toEncoding (Attributes {..}) =
    pairs
      ("COLOR_0" .= _attributesCOLOR0 <> "JOINTS_0" .= _attributesJOINTS0 <>
       "NORMAL" .=
       _attributesNORMAL <>
       "WEIGHTS_0" .=
       _attributesWEIGHTS0 <>
       "TEXCOORD_1" .=
       _attributesTEXCOORD1 <>
       "TEXCOORD_0" .=
       _attributesTEXCOORD0 <>
       "TANGENT" .=
       _attributesTANGENT <>
       "POSITION" .=
       _attributesPOSITION)


data KHRDracoMeshCompression = KHRDracoMeshCompression {
    _kHRDracoMeshCompressionBufferView :: Int,
    _kHRDracoMeshCompressionAttributes :: Attributes
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON KHRDracoMeshCompression where
  parseJSON (Object v) = KHRDracoMeshCompression <$> v .:   "bufferView" <*> v .:   "attributes"
  parseJSON _          = mzero


instance ToJSON KHRDracoMeshCompression where
  toJSON     (KHRDracoMeshCompression {..}) = object ["bufferView" .= _kHRDracoMeshCompressionBufferView, "attributes" .= _kHRDracoMeshCompressionAttributes]
  toEncoding (KHRDracoMeshCompression {..}) = pairs  ("bufferView" .= _kHRDracoMeshCompressionBufferView<>"attributes" .= _kHRDracoMeshCompressionAttributes)


data TextureInfo = TextureInfo {
    _textureInfoIndex :: Double
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON TextureInfo where
  parseJSON (Object v) = TextureInfo <$> v .:   "index"
  parseJSON _          = mzero


instance ToJSON TextureInfo where
  toJSON     (TextureInfo {..}) = object ["index" .= _textureInfoIndex]
  toEncoding (TextureInfo {..}) = pairs  ("index" .= _textureInfoIndex)


data KHRMaterialsPbrSpecularGlossiness = KHRMaterialsPbrSpecularGlossiness {
    _kHRMaterialsPbrSpecularGlossinessGlossinessFactor :: Maybe Double,
    _kHRMaterialsPbrSpecularGlossinessSpecularFactor :: (Maybe([Double])), -- number[3]
    _kHRMaterialsPbrSpecularGlossinessDiffuseFactor :: (Maybe ([Double])), -- number[4]
    _kHRMaterialsPbrSpecularGlossinessSpecularGlossinessTexture :: Maybe TextureInfo,
    _kHRMaterialsPbrSpecularGlossinessTextureInfo :: (Maybe (TextureInfo))
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON KHRMaterialsPbrSpecularGlossiness where
  parseJSON (Object v) =
    KHRMaterialsPbrSpecularGlossiness <$> v .:?? "glossinessFactor" <*>
    (v .:?? "specularFactor") <*>
    (v .:?? "diffuseFactor") <*>
    v .:?? "specularGlossinessTexture" <*>
    v .:?? "diffuseTexture"
  parseJSON _ = mzero


instance ToJSON KHRMaterialsPbrSpecularGlossiness where
  toJSON (KHRMaterialsPbrSpecularGlossiness {..}) =
    object
      [ "glossinessFactor" .= _kHRMaterialsPbrSpecularGlossinessGlossinessFactor
      , "specularFactor" .= _kHRMaterialsPbrSpecularGlossinessSpecularFactor
      , "diffuseFactor" .= _kHRMaterialsPbrSpecularGlossinessDiffuseFactor
      , "specularGlossinessTexture" .=
        _kHRMaterialsPbrSpecularGlossinessSpecularGlossinessTexture
      , "diffuseTexture" .= _kHRMaterialsPbrSpecularGlossinessTextureInfo
      ]
  toEncoding (KHRMaterialsPbrSpecularGlossiness {..}) =
    pairs
      ("glossinessFactor" .= _kHRMaterialsPbrSpecularGlossinessGlossinessFactor <>
       "specularFactor" .=
       _kHRMaterialsPbrSpecularGlossinessSpecularFactor <>
       "diffuseFactor" .=
       _kHRMaterialsPbrSpecularGlossinessDiffuseFactor <>
       "specularGlossinessTexture" .=
       _kHRMaterialsPbrSpecularGlossinessSpecularGlossinessTexture <>
       "diffuseTexture" .=
       _kHRMaterialsPbrSpecularGlossinessTextureInfo)


data KHRTextureTransform = KHRTextureTransform {
    _kHRTextureTransformRotation :: (Maybe Double),
    _kHRTextureTransformOffset   :: (Maybe ([Double])), -- array[2]
    _kHRTextureTransformScale    :: (Maybe ([Double])) -- array[2]
    -- FIXME: missing texCoord
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON KHRTextureTransform where
  parseJSON (Object v) = KHRTextureTransform <$> v .:?? "rotation" <*> v .:?? "offset" <*> v .:?? "scale"
  parseJSON _          = mzero


instance ToJSON KHRTextureTransform where
  toJSON (KHRTextureTransform {..}) =
    object
      [ "rotation" .= _kHRTextureTransformRotation
      , "offset" .= _kHRTextureTransformOffset
      , "scale" .= _kHRTextureTransformScale
      ]
  toEncoding (KHRTextureTransform {..}) =
    pairs
      ("rotation" .= _kHRTextureTransformRotation <> "offset" .=
       _kHRTextureTransformOffset <>
       "scale" .=
       _kHRTextureTransformScale)

newtype Uri = Uri {unUri :: Text} deriving (Show,NFData,Ord, Eq, Read, Generic)

data ImagesElt = ImagesElt {
    _imagesEltUri      :: Uri,
    _imagesEltMimeType :: Maybe Text,
    _imagesEltName     :: Maybe Text
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON ImagesElt where
  parseJSON (Object v) =
    ImagesElt <$> (Uri <$> (v .: "uri")) <*> v .:?? "mimeType" <*> v .:?? "name"
  parseJSON _ = mzero


instance ToJSON ImagesElt where
  toJSON (ImagesElt {..}) =
    object
      [ "uri" .= (unUri $ _imagesEltUri)
      , "mimeType" .= _imagesEltMimeType
      , "name" .= _imagesEltName
      ]
  toEncoding (ImagesElt {..}) =
    pairs
      ("uri" .= (unUri $ _imagesEltUri) <> "mimeType" .= _imagesEltMimeType <> "name" .=
       _imagesEltName)


data TexturesElt = TexturesElt {
    _texturesEltSampler :: Maybe Int,
    _texturesEltName    :: Maybe Text,
    _texturesEltSource  :: Int
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON TexturesElt where
  parseJSON (Object v) = TexturesElt <$> v .:?? "sampler" <*> v .:?? "name" <*> v .:   "source"
  parseJSON _          = mzero


instance ToJSON TexturesElt where
  toJSON (TexturesElt {..}) =
    object
      [ "sampler" .= _texturesEltSampler
      , "name" .= _texturesEltName
      , "source" .= _texturesEltSource
      ]
  toEncoding (TexturesElt {..}) =
    pairs
      ("sampler" .= _texturesEltSampler <> "name" .= _texturesEltName <> "source" .=
       _texturesEltSource)

data Interpolation
  = Interpolation_STEP
  | Interpolation_LINEAR
  | Interpolation_CUBICSPLINE
  | Interpolation_Unknown Text
  deriving (Show,NFData, Ord, Read, Eq, Generic)

instance FromJSON Interpolation where
  parseJSON (String t) = fromString t
    where
      fromString :: Text -> Parser Interpolation
      fromString "STEP"        = pure Interpolation_STEP
      fromString "LINEAR"      = pure Interpolation_LINEAR
      fromString "CUBICSPLINE" = pure Interpolation_CUBICSPLINE
      fromString o             = pure $ Interpolation_Unknown o
-- TODO: ToJSON

instance ToJSON Interpolation where


data MinFilter
  = MinFilter_NEAREST
  | MinFilter_LINEAR
  | MinFilter_NEAREST_MIPMAP_NEAREST
  | MinFilter_LINEAR_MIPMAP_NEAREST
  | MinFilter_NEAREST_MIPMAP_LINEAR
  | MinFilter_LINEAR_MIPMAP_LINEAR
  deriving (Show,NFData,Ord, Read, Bounded, Eq, Generic)

minFilterToEnum :: MinFilter -> Int
minFilterToEnum =
  \case
    MinFilter_NEAREST -> 9728
    MinFilter_LINEAR -> 9729
    MinFilter_NEAREST_MIPMAP_NEAREST -> 9984
    MinFilter_LINEAR_MIPMAP_NEAREST -> 9985
    MinFilter_NEAREST_MIPMAP_LINEAR -> 9986
    MinFilter_LINEAR_MIPMAP_LINEAR -> 9987

enumToMinFilter :: Int -> MinFilter
enumToMinFilter =
  \case
    9728 -> MinFilter_NEAREST
    9729 -> MinFilter_LINEAR
    9984 -> MinFilter_NEAREST_MIPMAP_NEAREST
    9985 -> MinFilter_LINEAR_MIPMAP_NEAREST
    9986 -> MinFilter_NEAREST_MIPMAP_LINEAR
    9987 -> MinFilter_LINEAR_MIPMAP_LINEAR
    i -> error $ "gltf-hs: enumToMinFilter: " <> show i <> " not in expected range " <> show (fmap minFilterToEnum [minBound .. maxBound])

data MagFilter
  = MagFilter_NEAREST
  | MagFilter_LINEAR
  deriving (Show,NFData,Ord, Read, Bounded, Eq, Generic)

enumToMagFilter :: Int -> MagFilter
enumToMagFilter = \case
  9728 -> MagFilter_NEAREST
  9729 -> MagFilter_LINEAR
  i -> error $ "gltf-hs: enumToMagFilter: " <> show i <> " not in expected range " <> show (fmap magFilterToEnum [minBound .. maxBound])

magFilterToEnum :: MagFilter -> Int
magFilterToEnum = \case
  MagFilter_NEAREST -> 9728
  MagFilter_LINEAR -> 9729

instance Enum MagFilter where
  fromEnum = magFilterToEnum
  toEnum = enumToMagFilter

instance Enum MinFilter where
  fromEnum = minFilterToEnum
  toEnum = enumToMinFilter

instance FromJSON MagFilter where
  parseJSON (Number n) = toInt n
    where
      toInt n' =
        if isInteger n'
          then case toBoundedInteger n' of
                 Just s  -> pure $ toEnum s
                 Nothing -> empty
          else empty
  parseJSON _ = empty

instance FromJSON MinFilter where
  parseJSON (Number n) = toInt n
    where
      toInt n' =
        if isInteger n'
          then case toBoundedInteger n' of
                 Just s  -> pure $ toEnum s
                 Nothing -> empty
          else empty
  parseJSON _ = empty

-- FIXME: ToJSON instances

data SamplersElt = SamplersElt
  { _samplersEltWrapS         :: Maybe Int
  , _samplersEltInterpolation :: Maybe Interpolation
  , _samplersEltMinFilter     :: Maybe MinFilter
  , _samplersEltWrapT         :: Maybe Int
  , _samplersEltMagFilter     :: Maybe MagFilter
  , _samplersEltInput         :: Maybe Int
  , _samplersEltName          :: Maybe Text
  , _samplersEltOutput        :: Maybe Int
  } deriving (Show,NFData,Ord, Eq, Generic)


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


-- instance ToJSON SamplersElt where
--   toJSON (SamplersElt {..}) =
--     object
--       [ "wrapS" .= _samplersEltWrapS
--       , "interpolation" .= _samplersEltInterpolation
--       , "minFilter" .= _samplersEltMinFilter
--       , "wrapT" .= _samplersEltWrapT
--       , "magFilter" .= _samplersEltMagFilter
--       , "input" .= _samplersEltInput
--       , "name" .= _samplersEltName
--       , "output" .= _samplersEltOutput
--       ]
--   toEncoding (SamplersElt {..}) =
--     pairs
--       ("wrapS" .= _samplersEltWrapS <> "interpolation" .=
--        _samplersEltInterpolation <>
--        "minFilter" .=
--        _samplersEltMinFilter <>
--        "wrapT" .=
--        _samplersEltWrapT <>
--        "magFilter" .=
--        _samplersEltMagFilter <>
--        "input" .=
--        _samplersEltInput <>
--        "name" .=
--        _samplersEltName <>
--        "output" .=
--        _samplersEltOutput)


data Extensions = Extensions
  { _extensionsKHRTextureTransform :: Maybe KHRTextureTransform
  , _extensionsKHRDracoMeshCompression ::Maybe KHRDracoMeshCompression
  , _extensionsKHRMaterialsPbrSpecularGlossiness :: Maybe KHRMaterialsPbrSpecularGlossiness
  } deriving (Show,NFData,Ord, Eq, Generic)


instance FromJSON Extensions where
  parseJSON (Object v) =
    Extensions <$> v .:?? "KHR_texture_transform" <*>
    v .:?? "KHR_draco_mesh_compression" <*>
    v .:?? "KHR_materials_pbrSpecularGlossiness"
  parseJSON _ = mzero


instance ToJSON Extensions where
  toJSON (Extensions {..}) =
    object
      [ "KHR_texture_transform" .= _extensionsKHRTextureTransform
      , "KHR_draco_mesh_compression" .= _extensionsKHRDracoMeshCompression
      , "KHR_materials_pbrSpecularGlossiness" .=
        _extensionsKHRMaterialsPbrSpecularGlossiness
      ]
  toEncoding (Extensions {..}) =
    pairs
      ("KHR_texture_transform" .= _extensionsKHRTextureTransform <>
       "KHR_draco_mesh_compression" .=
       _extensionsKHRDracoMeshCompression <>
       "KHR_materials_pbrSpecularGlossiness" .=
       _extensionsKHRMaterialsPbrSpecularGlossiness)


data TargetsElt = TargetsElt
  { _targetsEltNORMAL   :: Maybe Int
  , _targetsEltTANGENT  :: Maybe Int
  , _targetsEltPOSITION :: Int
  } deriving (Show,NFData,Ord, Eq, Generic)


instance FromJSON TargetsElt where
  parseJSON (Object v) =
    TargetsElt <$> v .:?? "NORMAL" <*> v .:?? "TANGENT" <*> v .: "POSITION"
  parseJSON _ = mzero


instance ToJSON TargetsElt where
  toJSON     (TargetsElt {..}) = object ["NORMAL" .= _targetsEltNORMAL, "TANGENT" .= _targetsEltTANGENT, "POSITION" .= _targetsEltPOSITION]
  toEncoding (TargetsElt {..}) = pairs  ("NORMAL" .= _targetsEltNORMAL<>"TANGENT" .= _targetsEltTANGENT<>"POSITION" .= _targetsEltPOSITION)


data PrimitiveMode =
   PrimitiveMode_POINTS
  | PrimitiveMode_LINES
  | PrimitiveMode_LINE_LOOP
  | PrimitiveMode_LINE_STRIP
  | PrimitiveMode_TRIANGLES
  | PrimitiveMode_TRIANGLE_STRIP
  | PrimitiveMode_TRIANGLE_FAN
  deriving (Show,NFData,Ord, Eq, Generic, Enum, Bounded)

instance FromJSON PrimitiveMode where
  parseJSON (Number n) = toInt n
    where
      toInt n =
        if isInteger n
          then case toBoundedInteger n of
                 Just s  -> pure $ toEnum s
                 Nothing -> empty
          else empty
  parseJSON _ = empty

data PrimitivesElt = PrimitivesElt {
    _primitivesEltExtensions :: Maybe Extensions,
    _primitivesEltMode       :: Maybe PrimitiveMode,
    _primitivesEltMaterial   :: Maybe Int,
    _primitivesEltIndices    :: Maybe Int64,
    _primitivesEltAttributes :: Attributes,
    _primitivesEltTargets    :: Maybe [TargetsElt]
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON PrimitivesElt where
  parseJSON (Object v) =
    PrimitivesElt <$> v .:?? "extensions" <*> v .:?? "mode" <*>
    v .:?? "material" <*>
    v .:?? "_indices" <*>
    v .: "attributes" <*>
    v .:?? "targets"
  parseJSON _ = mzero


-- instance ToJSON PrimitivesElt where
--   toJSON (PrimitivesElt {..}) =
--     object
--       [ "extensions" .= _primitivesEltExtensions
--       , "mode" .= _primitivesEltMode
--       , "material" .= _primitivesEltMaterial
--       , "_indices" .= _primitivesEltIndices
--       , "attributes" .= _primitivesEltAttributes
--       , "targets" .= _primitivesEltTargets
--       ]
--   toEncoding (PrimitivesElt {..}) =
    -- pairs
    --   ("extensions" .= _primitivesEltExtensions <> "mode" .= _primitivesEltMode <>
    --    "material" .=
    --    _primitivesEltMaterial <>
    --    "_indices" .=
    --    _primitivesEltIndices <>
    --    "attributes" .=
    --    _primitivesEltAttributes <>
    --    "targets" .=
    --    _primitivesEltTargets)


data MeshesElt = MeshesElt {
    _meshesEltName       :: Maybe Text,
    _meshesEltPrimitives :: [PrimitivesElt],
    _meshesEltWeights    :: Maybe [Double]
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON MeshesElt where
  parseJSON (Object v) = MeshesElt <$> v .:?? "name" <*> v .:   "primitives" <*> v .:?? "weights"
  parseJSON _          = mzero


-- instance ToJSON MeshesElt where
--   toJSON (MeshesElt {..}) =
--     object
--       [ "name" .= _meshesEltName
--       , "primitives" .= _meshesEltPrimitives
--       , "weights" .= _meshesEltWeights
--       ]
--   toEncoding (MeshesElt {..}) =
--     pairs
--       ("name" .= _meshesEltName <> "primitives" .= _meshesEltPrimitives <>
--        "weights" .=
--        _meshesEltWeights)


data EmissiveTexture = EmissiveTexture {
    _emissiveTextureTexCoord :: Maybe Int,
    _emissiveTextureIndex    :: Int
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON EmissiveTexture where
  parseJSON (Object v) = EmissiveTexture <$> v .:?? "texCoord" <*> v .: "index"
  parseJSON _ = mzero


instance ToJSON EmissiveTexture where
  toJSON (EmissiveTexture {..}) =
    object
      ["texCoord" .= _emissiveTextureTexCoord, "index" .= _emissiveTextureIndex]
  toEncoding (EmissiveTexture {..}) =
    pairs
      ("texCoord" .= _emissiveTextureTexCoord <> "index" .= _emissiveTextureIndex)


data BaseColorTexture = BaseColorTexture {
    _baseColorTextureExtensions :: Maybe Extensions,
    _baseColorTextureTexCoord   :: Maybe Int,
    _baseColorTextureIndex      :: Int
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON BaseColorTexture where
  parseJSON (Object v) =
    BaseColorTexture <$> v .:?? "extensions" <*> v .:?? "texCoord" <*>
    v .: "index"
  parseJSON _ = mzero


instance ToJSON BaseColorTexture where
  toJSON (BaseColorTexture {..}) =
    object
      [ "extensions" .= _baseColorTextureExtensions
      , "texCoord" .= _baseColorTextureTexCoord
      , "index" .= _baseColorTextureIndex
      ]
  toEncoding (BaseColorTexture {..}) =
    pairs
      ("extensions" .= _baseColorTextureExtensions <> "texCoord" .=
       _baseColorTextureTexCoord <>
       "index" .=
       _baseColorTextureIndex)


data PbrMetallicRoughness = PbrMetallicRoughness
  { _pbrMetallicRoughnessMetallicFactor           :: Maybe Double
  , _pbrMetallicRoughnessMetallicRoughnessTexture :: Maybe TextureInfo
  , _pbrMetallicRoughnessBaseColorTexture         :: Maybe BaseColorTexture
  , _pbrMetallicRoughnessBaseColorFactor          :: (Maybe ([Double])) -- ^ array[4]
  , _pbrMetallicRoughnessRoughnessFactor          :: Maybe Double
  } deriving (Show,NFData,Ord, Eq, Generic)


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
      [ "metallicFactor" .= _pbrMetallicRoughnessMetallicFactor
      , "metallicRoughnessTexture" .=
        _pbrMetallicRoughnessMetallicRoughnessTexture
      , "baseColorTexture" .= _pbrMetallicRoughnessBaseColorTexture
      , "baseColorFactor" .= _pbrMetallicRoughnessBaseColorFactor
      , "roughnessFactor" .= _pbrMetallicRoughnessRoughnessFactor
      ]
  toEncoding (PbrMetallicRoughness {..}) =
    pairs
      ("metallicFactor" .= _pbrMetallicRoughnessMetallicFactor <>
       "metallicRoughnessTexture" .=
       _pbrMetallicRoughnessMetallicRoughnessTexture <>
       "baseColorTexture" .=
       _pbrMetallicRoughnessBaseColorTexture <>
       "baseColorFactor" .=
       _pbrMetallicRoughnessBaseColorFactor <>
       "roughnessFactor" .=
       _pbrMetallicRoughnessRoughnessFactor)


data NormalTexture = NormalTexture {
    _normalTextureScale :: Maybe Double,
    _normalTextureIndex :: Int
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON NormalTexture where
  parseJSON (Object v) = NormalTexture <$> v .:?? "scale" <*> v .:   "index"
  parseJSON _          = mzero


instance ToJSON NormalTexture where
  toJSON     (NormalTexture {..}) = object ["scale" .= _normalTextureScale, "index" .= _normalTextureIndex]
  toEncoding (NormalTexture {..}) = pairs  ("scale" .= _normalTextureScale<>"index" .= _normalTextureIndex)


data MaterialsElt = MaterialsElt
  { _materialsEltEmissiveTexture      :: Maybe EmissiveTexture
  , _materialsEltOcclusionTexture     :: Maybe TextureInfo
  , _materialsEltExtensions           :: Maybe Extensions
  , _materialsEltDoubleSided          :: Maybe Bool
  , _materialsEltPbrMetallicRoughness :: Maybe PbrMetallicRoughness
  , _materialsEltEmissiveFactor       :: (Maybe ([Double]))
  , _materialsEltAlphaCutoff          :: Maybe Double
  , _materialsEltNormalTexture        :: Maybe NormalTexture
  , _materialsEltName                 :: Maybe Text
  , _materialsEltAlphaMode            :: Maybe Text
  } deriving (Show,NFData,Ord, Eq, Generic)


instance FromJSON MaterialsElt where
  parseJSON (Object v) =
    MaterialsElt <$> v .:?? "emissiveTexture" <*> v .:?? "occlusionTexture" <*>
    v .:?? "extensions" <*>
    v .:?? "doubleSided" <*>
    v .:?? "pbrMetallicRoughness" <*>
    v .:?? "emissiveFactor" <*>
    v .:?? "alphaCutoff" <*>
    v .:?? "_normalTexture" <*>
    v .:?? "name" <*>
    v .:?? "alphaMode"
  parseJSON _ = mzero


instance ToJSON MaterialsElt where
  toJSON (MaterialsElt {..}) =
    object
      [ "emissiveTexture" .= _materialsEltEmissiveTexture
      , "occlusionTexture" .= _materialsEltOcclusionTexture
      , "extensions" .= _materialsEltExtensions
      , "doubleSided" .= _materialsEltDoubleSided
      , "pbrMetallicRoughness" .= _materialsEltPbrMetallicRoughness
      , "emissiveFactor" .= _materialsEltEmissiveFactor
      , "alphaCutoff" .= _materialsEltAlphaCutoff
      , "_normalTexture" .= _materialsEltNormalTexture
      , "name" .= _materialsEltName
      , "alphaMode" .= _materialsEltAlphaMode
      ]
  toEncoding (MaterialsElt {..}) =
    pairs
      ("emissiveTexture" .= _materialsEltEmissiveTexture <> "occlusionTexture" .=
       _materialsEltOcclusionTexture <>
       "extensions" .=
       _materialsEltExtensions <>
       "doubleSided" .=
       _materialsEltDoubleSided <>
       "pbrMetallicRoughness" .=
       _materialsEltPbrMetallicRoughness <>
       "emissiveFactor" .=
       _materialsEltEmissiveFactor <>
       "alphaCutoff" .=
       _materialsEltAlphaCutoff <>
       "_normalTexture" .=
       _materialsEltNormalTexture <>
       "name" .=
       _materialsEltName <>
       "alphaMode" .=
       _materialsEltAlphaMode)


data Extras = Extras {
    _extrasAuthor  :: Text,
    _extrasTitle   :: Text,
    _extrasLicense :: Text
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON Extras where
  parseJSON (Object v) =
    Extras <$> v .: "author" <*> v .: "title" <*> v .: "license"
  parseJSON _ = mzero


instance ToJSON Extras where
  toJSON (Extras {..}) =
    object
      [ "author" .= _extrasAuthor
      , "title" .= _extrasTitle
      , "license" .= _extrasLicense
      ]
  toEncoding (Extras {..}) =
    pairs
      ("author" .= _extrasAuthor <> "title" .= _extrasTitle <> "license" .=
       _extrasLicense)


data Asset = Asset {
    _assetCopyright :: Maybe Text,
    _assetVersion   :: Text,
    _assetGenerator :: Maybe Text,
    _assetExtras    :: Maybe Extras
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON Asset where
  parseJSON (Object v) = Asset <$> v .:?? "copyright" <*> v .:   "version" <*> v .:?? "generator" <*> v .:?? "extras"
  parseJSON _          = mzero


instance ToJSON Asset where
  toJSON (Asset {..}) =
    object
      [ "copyright" .= _assetCopyright
      , "version" .= _assetVersion
      , "generator" .= _assetGenerator
      , "extras" .= _assetExtras
      ]
  toEncoding (Asset {..}) =
    pairs
      ("copyright" .= _assetCopyright <> "version" .= _assetVersion <> "generator" .=
       _assetGenerator <>
       "extras" .=
       _assetExtras)


data BuffersElt = BuffersElt {
    _buffersEltUri        :: Uri,
    _buffersEltName       :: Maybe Text,
    _buffersEltByteLength :: Double
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON BuffersElt where
  parseJSON (Object v) = BuffersElt <$> (Uri <$> v .:   "uri") <*> v .:?? "name" <*> v .:   "byteLength"
  parseJSON _          = mzero


instance ToJSON BuffersElt where
  toJSON (BuffersElt {..}) =
    object
      [ "uri" .= (unUri _buffersEltUri)
      , "name" .= _buffersEltName
      , "byteLength" .= _buffersEltByteLength
      ]
  toEncoding (BuffersElt {..}) =
    pairs
      ("uri" .= (unUri _buffersEltUri) <> "name" .= _buffersEltName <> "byteLength" .=
       _buffersEltByteLength)


data SkinsElt = SkinsElt {
    _skinsEltJoints              :: [Double],
    _skinsEltSkeleton            :: Double,
    _skinsEltName                :: Text,
    _skinsEltInverseBindMatrices :: Double
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON SkinsElt where
  parseJSON (Object v) =
    SkinsElt <$> v .: "joints" <*> v .: "skeleton" <*> v .: "name" <*>
    v .: "inverseBindMatrices"
  parseJSON _ = mzero


instance ToJSON SkinsElt where
  toJSON (SkinsElt {..}) =
    object
      [ "joints" .= _skinsEltJoints
      , "skeleton" .= _skinsEltSkeleton
      , "name" .= _skinsEltName
      , "inverseBindMatrices" .= _skinsEltInverseBindMatrices
      ]
  toEncoding (SkinsElt {..}) =
    pairs
      ("joints" .= _skinsEltJoints <> "skeleton" .= _skinsEltSkeleton <> "name" .=
       _skinsEltName <>
       "inverseBindMatrices" .=
       _skinsEltInverseBindMatrices)


data Values = Values {
    _valuesByteOffset :: Int,
    _valuesBufferView :: Int
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON Values where
  parseJSON (Object v) = Values <$> v .:   "byteOffset" <*> v .:   "bufferView"
  parseJSON _          = mzero


instance ToJSON Values where
  toJSON     (Values {..}) = object ["byteOffset" .= _valuesByteOffset, "bufferView" .= _valuesBufferView]
  toEncoding (Values {..}) = pairs  ("byteOffset" .= _valuesByteOffset<>"bufferView" .= _valuesBufferView)


data Indices = Indices {
    _indicesByteOffset    :: Int,
    _indicesBufferView    :: Int,
    _indicesComponentType :: ComponentType
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON Indices where
  parseJSON (Object v) = Indices <$> v .:   "byteOffset" <*> v .:   "bufferView" <*> v .:   "componentType"
  parseJSON _          = mzero


-- instance ToJSON Indices where
--   toJSON (Indices {..}) =
--     object
--       [ "byteOffset" .= _indicesByteOffset
--       , "bufferView" .= _indicesBufferView
--       , "componentType" .= _indicesComponentType
--       ]
--   toEncoding (Indices {..}) =
--     pairs
--       ("byteOffset" .= _indicesByteOffset <> "bufferView" .= _indicesBufferView <>
--        "componentType" .=
--        _indicesComponentType)


data Sparse = Sparse {
    _sparseValues  :: Values,
    _sparseCount   :: Int,
    _sparseIndices :: Indices
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON Sparse where
  parseJSON (Object v) = Sparse <$> v .:   "values" <*> v .:   "count" <*> v .:   "indices"
  parseJSON _          = mzero


-- instance ToJSON Sparse where
--   toJSON     (Sparse {..}) = object ["values" .= _sparseValues, "count" .= _sparseCount, "indices" .= _sparseIndices]
--   toEncoding (Sparse {..}) = pairs  ("values" .= _sparseValues<>"count" .= _sparseCount<>"indices" .= _sparseIndices)

-- | Component type and size in bytes
-- 5120 (BYTE)	1
-- 5121(UNSIGNED_BYTE)	1
-- 5122 (SHORT)	2
-- 5123 (UNSIGNED_SHORT)	2
-- 5125 (UNSIGNED_INT)	4
-- 5126 (FLOAT)	4

data ComponentType =
  ComponentType_BYTE
  | ComponentType_UNSIGNED_BYTE
  | ComponentType_SHORT
  | ComponentType_UNSIGNED_SHORT
  | ComponentType_UNSIGNED_INT
  | ComponentType_FLOAT
  deriving (Show,NFData,Ord, Eq, Bounded, Enum, Generic)

compenentTypeFromInt :: Int -> ComponentType
compenentTypeFromInt = \case
  5120 -> ComponentType_BYTE
  5121 -> ComponentType_UNSIGNED_BYTE
  5122 -> ComponentType_SHORT
  5123 -> ComponentType_UNSIGNED_SHORT
  5125 -> ComponentType_UNSIGNED_INT
  5126 -> ComponentType_FLOAT
  i -> error $ "gltf-hs: compenentTypeFromInt: " <> show i <> " not in expected range " <> show (fmap compenentTypeToInt [minBound .. maxBound])

compenentTypeToInt :: ComponentType -> Int
compenentTypeToInt = \case
  ComponentType_BYTE -> 5120
  ComponentType_UNSIGNED_BYTE -> 5121
  ComponentType_SHORT -> 5122
  ComponentType_UNSIGNED_SHORT -> 5123
  ComponentType_UNSIGNED_INT -> 5125
  ComponentType_FLOAT -> 5126

instance FromJSON ComponentType where
  parseJSON (Number n) = toInt n
    where
      toInt n' =
        if isInteger n'
          then case (toBoundedInteger n' :: Maybe Int) of
                 Just s  -> pure $ compenentTypeFromInt s
                 Nothing -> empty
          else empty
  parseJSON _ = empty

--FIXME: ToJson instances

data Component
  = Component_SCALAR
  | Component_VEC2
  | Component_VEC3
  | Component_VEC4
  | Component_MAT2
  | Component_MAT3
  | Component_MAT4
  deriving (Show,NFData,Ord,Eq,Generic)

nComponents :: Component -> Int
nComponents = \case
  Component_SCALAR -> 1
  Component_VEC2 -> 2
  Component_VEC3 -> 3
  Component_VEC4 -> 4
  Component_MAT2 -> 4
  Component_MAT3 -> 9
  Component_MAT4 -> 16

instance FromJSON Component where
  parseJSON (String s) = fromString s
    where
      fromString "SCALAR" = pure Component_SCALAR
      fromString "VEC2"   = pure Component_VEC2
      fromString "VEC3"   = pure Component_VEC3
      fromString "VEC4"   = pure Component_VEC4
      fromString "MAT2"   = pure Component_MAT2
      fromString "MAT3"   = pure Component_MAT3
      fromString "MAT4"   = pure Component_MAT4
  parseJSON _ = empty

-- FIXME: ToJson

data AccessorsElt = AccessorsElt {
    _accessorsEltMax           :: (Maybe [Double]),
    _accessorsEltByteOffset    :: Maybe Int,
    _accessorsEltBufferView    :: (Maybe Int),
    _accessorsEltCount         :: Int,
    _accessorsEltSparse        :: Maybe Sparse,
    _accessorsEltName          :: Maybe Text,
    _accessorsEltComponentType :: ComponentType,
    _accessorsEltMin           :: (Maybe ([Double])),
    _accessorsEltType          :: Component
  } deriving (Show,NFData,Ord,Eq,Generic)


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


-- instance ToJSON AccessorsElt where
--   toJSON (AccessorsElt {..}) =
--     object
--       [ "max" .= _accessorsEltMax
--       , "byteOffset" .= _accessorsEltByteOffset
--       , "bufferView" .= _accessorsEltBufferView
--       , "count" .= _accessorsEltCount
--       , "sparse" .= _accessorsEltSparse
--       , "name" .= _accessorsEltName
--       , "componentType" .= _accessorsEltComponentType
--       , "min" .= _accessorsEltMin
--       , "type" .= _accessorsEltType
--       ]
  -- toEncoding (AccessorsElt {..}) =
  --   pairs
  --     ("max" .= _accessorsEltMax <> "byteOffset" .= _accessorsEltByteOffset <>
  --      "bufferView" .=
  --      _accessorsEltBufferView <>
  --      "count" .=
  --      _accessorsEltCount <>
  --      "sparse" .=
  --      _accessorsEltSparse <>
  --      "name" .=
  --      _accessorsEltName <>
  --      "componentType" .=
  --      _accessorsEltComponentType <>
  --      "min" .=
  --      _accessorsEltMin <>
  --      "type" .=
  --      _accessorsEltType
      -- )


data Orthographic = Orthographic {
    _orthographicZfar  :: Double,
    _orthographicZnear :: Double,
    _orthographicXmag  :: Double,
    _orthographicYmag  :: Double
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON Orthographic where
  parseJSON (Object v) = Orthographic <$> v .:   "zfar" <*> v .:   "znear" <*> v .:   "xmag" <*> v .:   "ymag"
  parseJSON _          = mzero


instance ToJSON Orthographic where
  toJSON (Orthographic {..}) =
    object
      [ "zfar" .= _orthographicZfar
      , "znear" .= _orthographicZnear
      , "xmag" .= _orthographicXmag
      , "ymag" .= _orthographicYmag
      ]
  toEncoding (Orthographic {..}) =
    pairs
      ("zfar" .= _orthographicZfar <> "znear" .= _orthographicZnear <> "xmag" .=
       _orthographicXmag <>
       "ymag" .=
       _orthographicYmag)


data Perspective = Perspective {
    _perspectiveZfar        :: Double,
    _perspectiveZnear       :: Double,
    _perspectiveAspectRatio :: Maybe Double,
    _perspectiveYfov        :: Double
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON Perspective where
  parseJSON (Object v) = Perspective <$> v .:   "zfar" <*> v .:   "znear" <*> v .:?? "aspectRatio" <*> v .:   "yfov"
  parseJSON _          = mzero


instance ToJSON Perspective where
  toJSON (Perspective {..}) =
    object
      [ "zfar" .= _perspectiveZfar
      , "znear" .= _perspectiveZnear
      , "aspectRatio" .= _perspectiveAspectRatio
      , "yfov" .= _perspectiveYfov
      ]
  toEncoding (Perspective {..}) =
    pairs
      ("zfar" .= _perspectiveZfar <> "znear" .= _perspectiveZnear <> "aspectRatio" .=
       _perspectiveAspectRatio <>
       "yfov" .=
       _perspectiveYfov)


data CamerasElt = CamerasElt
  { _camerasEltOrthographic :: Maybe Orthographic
  , _camerasEltPerspective  :: Maybe Perspective
  , _camerasEltName         :: Maybe Text
  , _camerasEltType         :: Text
  } deriving (Show,NFData,Ord, Eq, Generic)


instance FromJSON CamerasElt where
  parseJSON (Object v) = CamerasElt <$> v .:?? "orthographic" <*> v .:?? "perspective" <*> v .:?? "name" <*> v .:   "type"
  parseJSON _          = mzero


instance ToJSON CamerasElt where
  toJSON (CamerasElt {..}) =
    object
      [ "orthographic" .= _camerasEltOrthographic
      , "perspective" .= _camerasEltPerspective
      , "name" .= _camerasEltName
      , "type" .= _camerasEltType
      ]
  toEncoding (CamerasElt {..}) =
    pairs
      ("orthographic" .= _camerasEltOrthographic <> "perspective" .=
       _camerasEltPerspective <>
       "name" .=
       _camerasEltName <>
       "type" .=
       _camerasEltType)


data ScenesElt = ScenesElt {
    _scenesEltName  :: Maybe Text,
    _scenesEltNodes :: [Double]
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON ScenesElt where
  parseJSON (Object v) = ScenesElt <$> v .:?? "name" <*> v .:   "nodes"
  parseJSON _          = mzero


instance ToJSON ScenesElt where
  toJSON (ScenesElt {..}) =
    object ["name" .= _scenesEltName, "nodes" .= _scenesEltNodes]
  toEncoding (ScenesElt {..}) =
    pairs ("name" .= _scenesEltName <> "nodes" .= _scenesEltNodes)


data Target = Target {
    _targetPath :: Text,
    _targetNode :: Int
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON Target where
  parseJSON (Object v) = Target <$> v .:   "path" <*> v .:   "node"
  parseJSON _          = mzero


instance ToJSON Target where
  toJSON (Target {..}) = object ["path" .= _targetPath, "node" .= _targetNode]
  toEncoding (Target {..}) =
    pairs ("path" .= _targetPath <> "node" .= _targetNode)


data ChannelsElt = ChannelsElt {
    _channelsEltSampler :: Int,
    _channelsEltTarget  :: Target
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON ChannelsElt where
  parseJSON (Object v) = ChannelsElt <$> v .:   "sampler" <*> v .:   "target"
  parseJSON _          = mzero


instance ToJSON ChannelsElt where
  toJSON (ChannelsElt {..}) =
    object ["sampler" .= _channelsEltSampler, "target" .= _channelsEltTarget]
  toEncoding (ChannelsElt {..}) =
    pairs ("sampler" .= _channelsEltSampler <> "target" .= _channelsEltTarget)


data AnimationsElt = AnimationsElt
  { _animationsEltSamplers :: [SamplersElt]
  , _animationsEltChannels :: [ChannelsElt]
  , _animationsEltName     :: Maybe Text
  } deriving (Show,NFData,Ord, Eq, Generic)


instance FromJSON AnimationsElt where
  parseJSON (Object v) =
    AnimationsElt <$> v .: "samplers" <*> v .: "channels" <*> v .:?? "name"
  parseJSON _ = mzero


-- instance ToJSON AnimationsElt where
--   toJSON     (AnimationsElt {..}) = object ["samplers" .= _animationsEltSamplers, "channels" .= _animationsEltChannels, "name" .= _animationsEltName]
--   toEncoding (AnimationsElt {..}) = pairs  ("samplers" .= _animationsEltSamplers<>"channels" .= _animationsEltChannels<>"name" .= _animationsEltName)


data NodesElt = NodesElt {
    _nodesEltRotation    :: (Maybe ([Double])),
    _nodesEltScale       :: (Maybe ([Double])),
    _nodesEltChildren    :: Maybe [Int],
    _nodesEltMatrix      :: (Maybe ([Double])),
    _nodesEltSkin        :: Maybe Int,
    _nodesEltName        :: Maybe Text,
    _nodesEltTranslation :: (Maybe ([Double])),
    _nodesEltMesh        :: Maybe Int,
    _nodesEltCamera      :: Maybe Int
  } deriving (Show,NFData,Ord,Eq,Generic)


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
      [ "rotation" .= _nodesEltRotation
      , "scale" .= _nodesEltScale
      , "children" .= _nodesEltChildren
      , "matrix" .= _nodesEltMatrix
      , "skin" .= _nodesEltSkin
      , "name" .= _nodesEltName
      , "translation" .= _nodesEltTranslation
      , "mesh" .= _nodesEltMesh
      , "camera" .= _nodesEltCamera
      ]
  toEncoding (NodesElt {..}) =
    pairs
      ("rotation" .= _nodesEltRotation <> "scale" .= _nodesEltScale <> "children" .=
       _nodesEltChildren <>
       "matrix" .=
       _nodesEltMatrix <>
       "skin" .=
       _nodesEltSkin <>
       "name" .=
       _nodesEltName <>
       "translation" .=
       _nodesEltTranslation <>
       "mesh" .=
       _nodesEltMesh <>
       "camera" .=
       _nodesEltCamera)


data BufferViewsElt = BufferViewsElt {
    _bufferViewsEltByteOffset :: Maybe Int,
    _bufferViewsEltName       :: (Maybe Text),
    _bufferViewsEltByteStride :: Maybe Int,
    _bufferViewsEltBuffer     :: Int,
    _bufferViewsEltByteLength :: Int,
    _bufferViewsEltTarget     :: Maybe Int
  } deriving (Show,NFData,Ord,Eq,Generic)


instance FromJSON BufferViewsElt where
  parseJSON (Object v) = BufferViewsElt <$> v .:?? "byteOffset" <*> v .:?? "name" <*> v .:?? "byteStride" <*> v .:   "buffer" <*> v .:   "byteLength" <*> v .:?? "target"
  parseJSON _          = mzero


instance ToJSON BufferViewsElt where
  toJSON (BufferViewsElt {..}) =
    object
      [ "byteOffset" .= _bufferViewsEltByteOffset
      , "name" .= _bufferViewsEltName
      , "byteStride" .= _bufferViewsEltByteStride
      , "buffer" .= _bufferViewsEltBuffer
      , "byteLength" .= _bufferViewsEltByteLength
      , "target" .= _bufferViewsEltTarget
      ]
  toEncoding (BufferViewsElt {..}) =
    pairs
      ("byteOffset" .= _bufferViewsEltByteOffset <> "name" .= _bufferViewsEltName <>
       "byteStride" .=
       _bufferViewsEltByteStride <>
       "buffer" .=
       _bufferViewsEltBuffer <>
       "byteLength" .=
       _bufferViewsEltByteLength <>
       "target" .=
       _bufferViewsEltTarget)


data TopLevel = TopLevel
  { _topLevelImages             :: (([ImagesElt]))
  , _topLevelTextures           :: (([TexturesElt]))
  , _topLevelSamplers           :: (([SamplersElt]))
  , _topLevelMeshes             :: [MeshesElt]
  , _topLevelExtensionsUsed     :: (([Text]))
  , _topLevelMaterials          :: (([MaterialsElt]))
  , _topLevelAsset              :: Asset
  , _topLevelBuffers            :: [BuffersElt]
  , _topLevelExtensionsRequired :: (([Text]))
  , _topLevelSkins              :: (([SkinsElt]))
  , _topLevelAccessors          :: [AccessorsElt]
  , _topLevelCameras            :: (([CamerasElt]))
  , _topLevelScenes             :: [ScenesElt]
  , _topLevelAnimations         :: (([AnimationsElt]))
  , _topLevelNodes              :: [NodesElt]
  , _topLevelBufferViews        :: [BufferViewsElt]
  , _topLevelScene              :: Maybe Int
  } deriving (Show,NFData,Ord, Eq, Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) =
    TopLevel <$> (concat <$> (v .:?? "images")) <*>
    (concat <$> v .:?? "textures") <*>
    (concat <$> v .:?? "samplers") <*>
    v .: "meshes" <*>
    (concat <$> v .:?? "extensionsUsed") <*>
    (concat <$> v .:?? "materials") <*>
    v .: "asset" <*>
    v .: "buffers" <*>
    (concat <$> v .:?? "extensionsRequired") <*>
    (concat <$> v .:?? "skins") <*>
    v .: "accessors" <*>
    (concat <$> v .:?? "cameras") <*>
    v .: "scenes" <*>
    (concat <$> v .:?? "animations") <*>
    v .: "nodes" <*>
    v .: "bufferViews" <*>
    v .:?? "scene"
  parseJSON _ = mzero


-- instance ToJSON TopLevel where
--   toJSON (TopLevel {..}) =
--     object
--       [ "images" .= _topLevelImages
--       , "textures" .= _topLevelTextures
--       , "samplers" .= _topLevelSamplers
--       , "meshes" .= _topLevelMeshes
--       , "extensionsUsed" .= _topLevelExtensionsUsed
--       , "materials" .= _topLevelMaterials
--       , "asset" .= _topLevelAsset
--       , "buffers" .= _topLevelBuffers
--       , "extensionsRequired" .= _topLevelExtensionsRequired
--       , "skins" .= _topLevelSkins
--       , "accessors" .= _topLevelAccessors
--       , "cameras" .= _topLevelCameras
--       , "scenes" .= _topLevelScenes
--       , "animations" .= _topLevelAnimations
--       , "nodes" .= _topLevelNodes
--       , "bufferViews" .= _topLevelBufferViews
--       , "scene" .= _topLevelScene
--       ]
  -- toEncoding (TopLevel {..}) =
  --   pairs
  --     ("images" .= _topLevelImages <> "textures" .= _topLevelTextures <>
  --      "samplers" .=
  --      _topLevelSamplers <>
  --      "meshes" .=
  --      _topLevelMeshes <>
  --      "extensionsUsed" .=
  --      _topLevelExtensionsUsed <>
  --      "materials" .=
  --      _topLevelMaterials <>
  --      "asset" .=
  --      _topLevelAsset <>
  --      "buffers" .=
  --      _topLevelBuffers <>
  --      "extensionsRequired" .=
  --      _topLevelExtensionsRequired <>
  --      "skins" .=
  --      _topLevelSkins <>
  --      "accessors" .=
  --      _topLevelAccessors <>
  --      "cameras" .=
  --      _topLevelCameras <>
  --      "scenes" .=
  --      _topLevelScenes <>
  --      "animations" .=
  --      _topLevelAnimations <>
  --      "nodes" .=
  --      _topLevelNodes <>
  --      "bufferViews" .=
  --      _topLevelBufferViews <>
  --      "scene" .=
  --      _topLevelScene
      -- )




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
                putStrLn msg
                exitFailure


mconcat <$> traverse
  makeLenses
  [''Attributes
  , ''KHRDracoMeshCompression
  , ''TextureInfo
  , ''KHRMaterialsPbrSpecularGlossiness
  , ''KHRTextureTransform
  , ''ImagesElt
  , ''TexturesElt
  , ''SamplersElt
  , ''Extensions
  , ''TargetsElt
  , ''PrimitivesElt
  , ''MeshesElt
  , ''EmissiveTexture
  , ''BaseColorTexture
  , ''PbrMetallicRoughness
  , ''NormalTexture
  , ''MaterialsElt
  , ''Extras
  , ''Asset
  , ''BuffersElt
  , ''SkinsElt
  , ''Values
  , ''Indices
  , ''Sparse
  , ''AccessorsElt
  , ''Orthographic
  , ''Perspective
  , ''CamerasElt
  , ''ScenesElt
  , ''Target
  , ''ChannelsElt
  , ''AnimationsElt
  , ''NodesElt
  , ''BufferViewsElt
  , ''TopLevel
  ]

mconcat <$> traverse
  makePrisms
  [''Interpolation
  , ''MinFilter
  , ''MagFilter
  , ''PrimitiveMode
  ]


data DecodingException
  = EmbeddedDecodingFailed Text
  | ResourceNotFound FilePath
  | UriTypeNotSupported Text
  deriving (Show,NFData, Eq, Ord, Generic, Exception)


resolveUri ::
     (MonadThrow m, MonadIO m) => Path Abs Dir -> Text -> m B.ByteString
resolveUri root t = do
  uri <- URI.mkURI t
  let mScheme = uri ^? uriScheme
      suppPaths = ["application", "image"]
      mpath = uri ^? uriPath
      miD =
        fmap (\(v1:v2) -> ((v1 ^. unRText) `elem` suppPaths, v2)) mpath
  iD <-
    case miD of
      Just (v, _imageType) -> pure v
      Nothing ->
        throwM . UriTypeNotSupported $ "neither scheme nor path present in URI!"
  if not (isJust $ join mScheme)
    then do
      rfile <- parseRelFile $ T.unpack t
      loadLocal (root </> rfile)
    else if iD
           then loadEmbedded t
           else throwM . UriTypeNotSupported $ t

chunkSize :: Int
chunkSize = 16384 * 4


loadLocal :: (MonadIO m, MonadThrow m) => Path Abs File -> m B.ByteString
loadLocal spth = do
  me <- liftIO $ doesFileExist pth
  if me
    then liftIO $ B.readFile pth
    else throwM $ ResourceNotFound pth
  where
    pth = toFilePath spth




loadEmbedded :: (MonadThrow m) => Text -> m B.ByteString
loadEmbedded t =
  case T.breakOnEnd ("base64," :: Text) t of -- FIXME: this is some slow-ass shit.
    (_, st) ->
      if "" /= st
        then go st
        else throwM $ EmbeddedDecodingFailed "Unexpected Uri!"
  where
    go st =
      case unBase64 <$> convertText st of
        Just bs -> pure bs
        Nothing ->
          throwM $ EmbeddedDecodingFailed "Could not convert value to Base64!"
