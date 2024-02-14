{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Hasura.SQL.WKTSpec
  ( spec,
  )
where

import Data.Either (isLeft)
import Hasura.Base.Error.TestInstances ()
import Hasura.Prelude
import Hasura.SQL.GeoJSON qualified as G
import Hasura.SQL.WKT
import Test.Hspec

spec :: Spec
spec = describe "WKT" do
  runSpec "Point" pointSpec
  runSpec "MultiPoint" multiPointSpec
  runSpec "LineString" lineStringSpec
  runSpec "MultiLineString" multiLineStringSpec
  runSpec "Polygon" polygonSpec
  runSpec "MultiPolygon" multiPolygonSpec
  runSpec "GeometryCollection" geometryCollectionSpec
  runNegativeSpec "3D types should error" negativeSpecs

negativeSpecs :: [G.Point]
negativeSpecs =
  [ mkPoint $ mkPosition 1 2 (Just 3),
    mkPoint $ mkPosition 1 2 (Just (-1))
  ]

runNegativeSpec :: forall a. (ToWKT a) => String -> [a] -> Spec
runNegativeSpec name = it name . traverse_ go
  where
    go wkt =
      getWKT <$> toWKT wkt `shouldSatisfy` isLeft

runSpec :: forall a. (ToWKT a) => String -> [(Text, a)] -> Spec
runSpec name = it name . traverse_ go
  where
    go (t, wkt) =
      getWKT <$> toWKT wkt `shouldBe` Right t

pointSpec :: [(Text, G.Point)]
pointSpec =
  [ ( "POINT (1.0 2.0)",
      mkPoint $ mkPosition 1 2 Nothing
    ),
    ( "POINT (1.1 2.2)",
      mkPoint $ mkPosition 1.1 2.2 Nothing
    ),
    ( "POINT (1.1 -2.2)",
      mkPoint $ mkPosition 1.1 (-2.2) Nothing
    )
  ]

multiPointSpec :: [(Text, G.MultiPoint)]
multiPointSpec =
  [ ( "MULTIPOINT (1.0 2.0)",
      mkMultiPoint [mkPosition 1 2 Nothing]
    ),
    ( "MULTIPOINT (1.1 2.2, 4.4 5.5)",
      mkMultiPoint
        [ mkPosition 1.1 2.2 Nothing,
          mkPosition 4.4 5.5 Nothing
        ]
    )
  ]

lineStringSpec :: [(Text, G.LineString)]
lineStringSpec =
  [ ( "LINESTRING (1.0 2.0, 3.0 4.0)",
      mkLineString
        [ mkPosition 1 2 Nothing,
          mkPosition 3 4 Nothing
        ]
    ),
    ( "LINESTRING (1.1 2.2, 4.4 5.5, 7.7 8.8)",
      mkLineString
        [ mkPosition 1.1 2.2 Nothing,
          mkPosition 4.4 5.5 Nothing,
          mkPosition 7.7 8.8 Nothing
        ]
    )
  ]

multiLineStringSpec :: [(Text, G.MultiLineString)]
multiLineStringSpec =
  [ ( "MULTILINESTRING ((1.1 2.2, 4.4 5.5, 7.7 8.8))",
      mkMultiLineString
        [ mkLineString
            [ mkPosition 1.1 2.2 Nothing,
              mkPosition 4.4 5.5 Nothing,
              mkPosition 7.7 8.8 Nothing
            ]
        ]
    ),
    ( "MULTILINESTRING ((1.1 2.2, 4.4 5.5, 7.7 8.8), (1.1 2.2, 4.4 5.5))",
      mkMultiLineString
        [ mkLineString
            [ mkPosition 1.1 2.2 Nothing,
              mkPosition 4.4 5.5 Nothing,
              mkPosition 7.7 8.8 Nothing
            ],
          mkLineString
            [ mkPosition 1.1 2.2 Nothing,
              mkPosition 4.4 5.5 Nothing
            ]
        ]
    )
  ]

polygonSpec :: [(Text, G.Polygon)]
polygonSpec =
  [ ( "POLYGON ((1.1 2.2, 4.4 5.5, 7.7 8.8, 1.1 2.2))",
      mkPolygon
        [ mkLinearRing
            [ mkPosition 1.1 2.2 Nothing,
              mkPosition 4.4 5.5 Nothing,
              mkPosition 7.7 8.8 Nothing
            ]
        ]
    ),
    ( "POLYGON ((1.1 2.2, 4.4 5.5, 7.7 8.8, 1.1 2.2), (1.1 2.2, 4.4 5.5, 7.7 8.8, 1.0 1.0, 1.1 2.2))",
      mkPolygon
        [ mkLinearRing
            [ mkPosition 1.1 2.2 Nothing,
              mkPosition 4.4 5.5 Nothing,
              mkPosition 7.7 8.8 Nothing
            ],
          mkLinearRing
            [ mkPosition 1.1 2.2 Nothing,
              mkPosition 4.4 5.5 Nothing,
              mkPosition 7.7 8.8 Nothing,
              mkPosition 1.0 1.0 Nothing
            ]
        ]
    )
  ]

multiPolygonSpec :: [(Text, G.MultiPolygon)]
multiPolygonSpec =
  [ ( "MULTIPOLYGON (((1.1 2.2, 4.4 5.5, 7.7 8.8, 1.1 2.2)))",
      mkMultiPolygon
        [ mkPolygon
            [ mkLinearRing
                [ mkPosition 1.1 2.2 Nothing,
                  mkPosition 4.4 5.5 Nothing,
                  mkPosition 7.7 8.8 Nothing
                ]
            ]
        ]
    ),
    ( "MULTIPOLYGON (((1.1 2.2, 4.4 5.5, 7.7 8.8, 1.1 2.2)), ((1.1 2.2, 4.4 5.5, 7.7 8.8, 1.1 2.2), (1.1 2.2, 4.4 5.5, 7.7 8.8, 1.0 1.0, 1.1 2.2)))",
      mkMultiPolygon
        [ mkPolygon
            [ mkLinearRing
                [ mkPosition 1.1 2.2 Nothing,
                  mkPosition 4.4 5.5 Nothing,
                  mkPosition 7.7 8.8 Nothing
                ]
            ],
          mkPolygon
            [ mkLinearRing
                [ mkPosition 1.1 2.2 Nothing,
                  mkPosition 4.4 5.5 Nothing,
                  mkPosition 7.7 8.8 Nothing
                ],
              mkLinearRing
                [ mkPosition 1.1 2.2 Nothing,
                  mkPosition 4.4 5.5 Nothing,
                  mkPosition 7.7 8.8 Nothing,
                  mkPosition 1.0 1.0 Nothing
                ]
            ]
        ]
    )
  ]

geometryCollectionSpec :: [(Text, G.GeometryCollection)]
geometryCollectionSpec =
  [ ( "GEOMETRYCOLLECTION (POINT (1.0 2.0), LINESTRING (3.0 4.0, 5.0 6.0))",
      mkGeometryCollection
        [ G.GPoint $ mkPoint $ mkPosition 1 2 Nothing,
          G.GLineString
            $ mkLineString
              [ mkPosition 3 4 Nothing,
                mkPosition 5 6 Nothing
              ]
        ]
    ),
    ( "GEOMETRYCOLLECTION (POINT (1.1 -2.2), MULTIPOINT (1.1 2.2, 4.4 5.5), LINESTRING (1.1 2.2, 4.4 5.5, 7.7 8.8), MULTILINESTRING ((1.1 2.2, 4.4 5.5, 7.7 8.8), (1.1 2.2, 4.4 5.5)), POLYGON ((1.1 2.2, 4.4 5.5, 7.7 8.8, 1.1 2.2), (1.1 2.2, 4.4 5.5, 7.7 8.8, 1.0 1.0, 1.1 2.2)), MULTIPOLYGON (((1.1 2.2, 4.4 5.5, 7.7 8.8, 1.1 2.2)), ((1.1 2.2, 4.4 5.5, 7.7 8.8, 1.1 2.2), (1.1 2.2, 4.4 5.5, 7.7 8.8, 1.0 1.0, 1.1 2.2))))",
      mkGeometryCollection
        [ G.GPoint $ mkPoint $ mkPosition 1.1 (-2.2) Nothing,
          G.GMultiPoint
            $ mkMultiPoint
              [ mkPosition 1.1 2.2 Nothing,
                mkPosition 4.4 5.5 Nothing
              ],
          G.GLineString
            $ mkLineString
              [ mkPosition 1.1 2.2 Nothing,
                mkPosition 4.4 5.5 Nothing,
                mkPosition 7.7 8.8 Nothing
              ],
          G.GMultiLineString
            $ mkMultiLineString
              [ mkLineString
                  [ mkPosition 1.1 2.2 Nothing,
                    mkPosition 4.4 5.5 Nothing,
                    mkPosition 7.7 8.8 Nothing
                  ],
                mkLineString
                  [ mkPosition 1.1 2.2 Nothing,
                    mkPosition 4.4 5.5 Nothing
                  ]
              ],
          G.GPolygon
            $ mkPolygon
              [ mkLinearRing
                  [ mkPosition 1.1 2.2 Nothing,
                    mkPosition 4.4 5.5 Nothing,
                    mkPosition 7.7 8.8 Nothing
                  ],
                mkLinearRing
                  [ mkPosition 1.1 2.2 Nothing,
                    mkPosition 4.4 5.5 Nothing,
                    mkPosition 7.7 8.8 Nothing,
                    mkPosition 1.0 1.0 Nothing
                  ]
              ],
          G.GMultiPolygon
            $ mkMultiPolygon
              [ mkPolygon
                  [ mkLinearRing
                      [ mkPosition 1.1 2.2 Nothing,
                        mkPosition 4.4 5.5 Nothing,
                        mkPosition 7.7 8.8 Nothing
                      ]
                  ],
                mkPolygon
                  [ mkLinearRing
                      [ mkPosition 1.1 2.2 Nothing,
                        mkPosition 4.4 5.5 Nothing,
                        mkPosition 7.7 8.8 Nothing
                      ],
                    mkLinearRing
                      [ mkPosition 1.1 2.2 Nothing,
                        mkPosition 4.4 5.5 Nothing,
                        mkPosition 7.7 8.8 Nothing,
                        mkPosition 1.0 1.0 Nothing
                      ]
                  ]
              ]
        ]
    )
  ]

mkGeometryCollection :: [G.Geometry] -> G.GeometryCollection
mkGeometryCollection = G.GeometryCollection . fmap (`G.GeometryWithCRS` Nothing)

mkMultiPolygon :: [G.Polygon] -> G.MultiPolygon
mkMultiPolygon = G.MultiPolygon

mkLinearRing :: [G.Position] -> G.LinearRing
mkLinearRing (x : y : z : xs) = G.LinearRing x y z xs
mkLinearRing _ = error "mkLinearRing requires >2 points"

mkPolygon :: [G.LinearRing] -> G.Polygon
mkPolygon = G.Polygon

mkMultiLineString :: [G.LineString] -> G.MultiLineString
mkMultiLineString = G.MultiLineString

mkLineString :: [G.Position] -> G.LineString
mkLineString (x : y : xs) = G.LineString x y xs
mkLineString _ = error "mkLineString requires >1 points"

mkMultiPoint :: [G.Position] -> G.MultiPoint
mkMultiPoint = G.MultiPoint

mkPoint :: G.Position -> G.Point
mkPoint = G.Point

mkPosition :: Double -> Double -> Maybe Double -> G.Position
mkPosition = G.Position
