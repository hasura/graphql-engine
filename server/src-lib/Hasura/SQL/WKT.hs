module Hasura.SQL.WKT
  ( ToWKT (..),
    WKT (..),
  )
where

import Data.List (intersperse)
import Hasura.Base.Error qualified as E
import Hasura.Prelude
import Hasura.SQL.GeoJSON qualified as G

newtype WKT = WKT {getWKT :: Text}

class ToWKT a where
  toWKT :: a -> Either E.QErr WKT

instance ToWKT G.Point where
  toWKT = mkWKT "POINT" . positionToText . G.unPoint

instance ToWKT G.MultiPoint where
  toWKT = mkWKT "MULTIPOINT" . commaSeparated . G.unMultiPoint

instance ToWKT G.LineString where
  toWKT = mkWKT "LINESTRING" . lineStringToText

instance ToWKT G.MultiLineString where
  toWKT =
    mkWKT "MULTILINESTRING"
      . fmap (mconcat . intersperse ", ")
      . traverse (fmap parens . lineStringToText)
      . G.unMultiLineString

instance ToWKT G.Polygon where
  toWKT =
    mkWKT "POLYGON"
      . fmap (mconcat . intersperse ", ")
      . traverse (fmap parens . linearRingToText)
      . G.unPolygon

instance ToWKT G.MultiPolygon where
  toWKT =
    mkWKT "MULTIPOLYGON"
      . fmap (mconcat . intersperse ", ")
      . traverse
        ( fmap (parens . mconcat . intersperse ", ")
            . traverse (fmap parens . linearRingToText)
            . G.unPolygon
        )
      . G.unMultiPolygon

instance ToWKT G.GeometryCollection where
  toWKT =
    mkWKT "GEOMETRYCOLLECTION"
      . fmap (mconcat . intersperse ", ")
      . traverse (fmap getWKT . toWKT . G._gwcGeom)
      . G.unGeometryCollection

instance ToWKT G.Geometry where
  toWKT =
    \case
      G.GPoint p -> toWKT p
      G.GMultiPoint m -> toWKT m
      G.GLineString l -> toWKT l
      G.GMultiLineString m -> toWKT m
      G.GPolygon p -> toWKT p
      G.GMultiPolygon m -> toWKT m
      G.GGeometryCollection g -> toWKT g

instance ToWKT G.GeometryWithCRS where
  toWKT = toWKT . G._gwcGeom

mkWKT :: Text -> Either E.QErr Text -> Either E.QErr WKT
mkWKT name args = WKT . wktFormat <$> args
  where
    wktFormat :: Text -> Text
    wktFormat a = name <> " " <> parens a

parens :: Text -> Text
parens t = "(" <> t <> ")"

commaSeparated :: [G.Position] -> Either E.QErr Text
commaSeparated = fmap (mconcat . intersperse ", ") . traverse positionToText

positionToText :: G.Position -> Either E.QErr Text
positionToText (G.Position x y mz) =
  case mz of
    Nothing -> pure $ tshow x <> " " <> tshow y
    Just _ -> Left $ E.err400 E.ParseFailed "3 dimmensional coordinates are not supported"

lineStringToText :: G.LineString -> Either E.QErr Text
lineStringToText (G.LineString ls1 ls2 lsRest) = commaSeparated (ls1 : ls2 : lsRest)

linearRingToText :: G.LinearRing -> Either E.QErr Text
linearRingToText (G.LinearRing p1 p2 p3 pRest) = commaSeparated (p1 : p2 : p3 : pRest <> [p1])
