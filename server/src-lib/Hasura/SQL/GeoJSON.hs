module Hasura.SQL.GeoJSON
  ( Position (..),
    Point (..),
    MultiPoint (..),
    LineString (..),
    LinearRing (..),
    MultiLineString (..),
    Polygon (..),
    MultiPolygon (..),
    Geometry (..),
    GeometryCollection (..),
    GeometryWithCRS (..),
  )
where

import Control.Monad
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.Aeson.Types qualified as J
import Data.Vector qualified as V
import Hasura.Prelude

-- Positions

data Position
  = Position !Double !Double !(Maybe Double)
  deriving (Show, Eq)

withParsedArray ::
  (J.FromJSON a) =>
  String ->
  (V.Vector a -> J.Parser b) ->
  J.Value ->
  J.Parser b
withParsedArray s fn =
  J.withArray s (mapM J.parseJSON >=> fn)

instance J.FromJSON Position where
  parseJSON = withParsedArray "Position" $ \arr ->
    if V.length arr < 2
      then fail "A Position needs at least 2 elements"
      else -- here we are ignoring anything past 3 elements

        return
          $ Position
            (arr `V.unsafeIndex` 0)
            (arr `V.unsafeIndex` 1)
            (arr V.!? 2)

instance J.ToJSON Position where
  toJSON (Position a b c) =
    J.toJSON $ a : b : maybeToList c

-- Point, Multipoint

newtype Point = Point {unPoint :: Position}
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

newtype MultiPoint = MultiPoint {unMultiPoint :: [Position]}
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

-- LineString, MultiLineString

data LineString = LineString
  { _lsFirst :: !Position,
    _lsSecond :: !Position,
    _lsRest :: ![Position]
  }
  deriving (Show, Eq)

instance J.ToJSON LineString where
  toJSON (LineString a b rest) =
    J.toJSON $ a : b : rest

instance J.FromJSON LineString where
  parseJSON = withParsedArray "LineString" $ \arr ->
    if V.length arr < 2
      then fail "A LineString needs at least 2 Positions"
      else -- here we are ignoring anything past 3 elements

        let fstPos = arr `V.unsafeIndex` 0
            sndPos = arr `V.unsafeIndex` 1
            rest = V.toList $ V.drop 2 arr
         in return $ LineString fstPos sndPos rest

newtype MultiLineString = MultiLineString {unMultiLineString :: [LineString]}
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

-- Polygon, MultiPolygon

data LinearRing = LinearRing
  { _pFirst :: !Position,
    _pSecond :: !Position,
    _pThird :: !Position,
    _pRest :: ![Position]
  }
  deriving (Show, Eq)

instance J.FromJSON LinearRing where
  parseJSON = withParsedArray "LinearRing" $ \arr ->
    if V.length arr < 4
      then fail "A LinearRing needs at least 4 Positions"
      else -- here we are ignoring anything past 3 elements
      do
        let fstPos = arr `V.unsafeIndex` 0
            sndPos = arr `V.unsafeIndex` 1
            thrPos = arr `V.unsafeIndex` 2
            rest = V.drop 3 arr
        let lastPos = V.last rest
        unless (fstPos == lastPos)
          $ fail "the first and last locations have to be equal for a LinearRing"
        return $ LinearRing fstPos sndPos thrPos $ V.toList $ V.init rest

instance J.ToJSON LinearRing where
  toJSON (LinearRing a b c rest) =
    J.toJSON $ (V.fromList [a, b, c] <> V.fromList rest) `V.snoc` a

newtype Polygon = Polygon {unPolygon :: [LinearRing]}
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

newtype MultiPolygon = MultiPolygon {unMultiPolygon :: [Polygon]}
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

-- GeometryCollection

data CRSNameProps = CRSNameProps
  { _cnpName :: !Text
  }
  deriving (Show, Eq, Generic)

data CRSLinkProps = CRSLinkProps
  { _clpHref :: !Text,
    _clpType :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

data CRS
  = CRSName !CRSNameProps
  | CRSLink !CRSLinkProps
  deriving (Show, Eq, Generic)

instance J.FromJSON CRSNameProps where
  parseJSON = J.genericParseJSON (J.aesonPrefix J.camelCase)

instance J.ToJSON CRSNameProps where
  toJSON = J.genericToJSON (J.aesonPrefix J.camelCase)
  toEncoding = J.genericToEncoding (J.aesonPrefix J.camelCase)

instance J.FromJSON CRSLinkProps where
  parseJSON = J.genericParseJSON (J.aesonPrefix J.camelCase)

instance J.ToJSON CRSLinkProps where
  toJSON = J.genericToJSON (J.aesonPrefix J.camelCase)
  toEncoding = J.genericToEncoding (J.aesonPrefix J.camelCase)

instance J.FromJSON CRS where
  parseJSON =
    J.genericParseJSON
      J.defaultOptions
        { J.constructorTagModifier = J.camelCase . drop 3,
          J.sumEncoding = J.TaggedObject "type" "properties"
        }

instance J.ToJSON CRS where
  toJSON =
    J.genericToJSON
      J.defaultOptions
        { J.constructorTagModifier = J.camelCase . drop 3,
          J.sumEncoding = J.TaggedObject "type" "properties"
        }
  toEncoding =
    J.genericToEncoding
      J.defaultOptions
        { J.constructorTagModifier = J.camelCase . drop 3,
          J.sumEncoding = J.TaggedObject "type" "properties"
        }

data GeometryWithCRS = GeometryWithCRS
  { _gwcGeom :: !Geometry,
    _gwcCrs :: !(Maybe CRS)
  }
  deriving (Show, Eq)

encToCoords :: (J.ToJSON a) => Text -> a -> Maybe CRS -> J.Value
encToCoords ty a Nothing =
  J.object ["type" J..= ty, "coordinates" J..= a]
encToCoords ty a (Just crs) =
  J.object ["type" J..= ty, "coordinates" J..= a, "crs" J..= crs]

instance J.ToJSON GeometryWithCRS where
  toJSON (GeometryWithCRS geom crsM) = case geom of
    GPoint o -> encToCoords "Point" o crsM
    GMultiPoint o -> encToCoords "MultiPoint" o crsM
    GLineString o -> encToCoords "LineString" o crsM
    GMultiLineString o -> encToCoords "MultiLineString" o crsM
    GPolygon o -> encToCoords "Polygon" o crsM
    GMultiPolygon o -> encToCoords "MultiPolygon" o crsM
    GGeometryCollection o ->
      J.object
        [ "type" J..= ("GeometryCollection" :: Text),
          "geometries" J..= o
        ]

instance J.FromJSON GeometryWithCRS where
  parseJSON = J.withObject "Geometry" $ \o -> do
    ty <- o J..: "type"
    geom <- case ty of
      "Point" -> GPoint <$> o J..: "coordinates"
      "MultiPoint" -> GMultiPoint <$> o J..: "coordinates"
      "LineString" -> GLineString <$> o J..: "coordinates"
      "MultiLineString" -> GMultiLineString <$> o J..: "coordinates"
      "Polygon" -> GPolygon <$> o J..: "coordinates"
      "MultiPolygon" -> GMultiPolygon <$> o J..: "coordinates"
      "GeometryCollection" -> GGeometryCollection <$> o J..: "geometries"
      _ -> fail $ "unexpected geometry type: " <> ty
    crsM <- o J..:? "crs"
    return $ GeometryWithCRS geom crsM

newtype GeometryCollection = GeometryCollection {unGeometryCollection :: [GeometryWithCRS]}
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

-- Geometry

data Geometry
  = GPoint !Point
  | GMultiPoint !MultiPoint
  | GLineString !LineString
  | GMultiLineString !MultiLineString
  | GPolygon !Polygon
  | GMultiPolygon !MultiPolygon
  | GGeometryCollection !GeometryCollection
  deriving (Show, Eq)
