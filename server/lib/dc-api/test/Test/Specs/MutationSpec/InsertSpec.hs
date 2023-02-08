module Test.Specs.MutationSpec.InsertSpec (spec) where

import Control.Lens (ix, (&), (.~), (?~), (^?))
import Data.Aeson qualified as J
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (maybeToList)
import Hasura.Backends.DataConnector.API
import Test.AgentAPI (mutationGuarded)
import Test.AgentDatasets (chinookTemplate, usesDataset)
import Test.Data (TestData (..))
import Test.Data qualified as Data
import Test.Expectations (mutationResponseShouldBe)
import Test.Sandwich (describe)
import Test.TestHelpers (AgentTestSpec, it)
import Prelude

spec :: TestData -> Capabilities -> AgentTestSpec
spec TestData {..} Capabilities {..} = describe "Insert Mutations" $ do
  usesDataset chinookTemplate $ it "can insert a single row" $ do
    let insertOperation = artistsInsertOperation & imoRows .~ take 1 newArtists
    let mutationRequest =
          Data.emptyMutationRequest
            & mrOperations .~ [InsertOperation insertOperation]
            & mrInsertSchema .~ [artistsInsertSchema]
    response <- mutationGuarded mutationRequest

    let expectedResult = MutationOperationResults 1 Nothing

    response `mutationResponseShouldBe` MutationResponse [expectedResult]

  usesDataset chinookTemplate $ it "can insert when the field names do not match the column names" $ do
    let row =
          RowObject . Data.mkFieldsMap $
            [ ("artist_name", mkColumnInsertFieldValue $ J.String "Taylor Swift")
            ]
    let insertOperation = artistsInsertOperation & imoRows .~ [row]
    let insertSchema =
          TableInsertSchema _tdArtistsTableName $
            Data.mkFieldsMap
              [ ("artist_name", ColumnInsert (_tdColumnInsertSchema _tdArtistsTableName "Name"))
              ]
    let mutationRequest =
          Data.emptyMutationRequest
            & mrOperations .~ [InsertOperation insertOperation]
            & mrInsertSchema .~ [insertSchema]
    response <- mutationGuarded mutationRequest

    let expectedResult = MutationOperationResults 1 Nothing

    response `mutationResponseShouldBe` MutationResponse [expectedResult]

  usesDataset chinookTemplate $ it "can insert multiple rows" $ do
    let insertOperation = artistsInsertOperation & imoRows .~ newArtists
    let mutationRequest =
          Data.emptyMutationRequest
            & mrOperations .~ [InsertOperation insertOperation]
            & mrInsertSchema .~ [artistsInsertSchema]
    response <- mutationGuarded mutationRequest

    let expectedResult = MutationOperationResults 4 Nothing

    response `mutationResponseShouldBe` MutationResponse [expectedResult]

  describe "returning" $ do
    usesDataset chinookTemplate $ it "can return all inserted columns including the auto-generated primary key" $ do
      let returning =
            Data.mkFieldsMap
              [ ("AlbumId", _tdColumnField _tdAlbumsTableName "AlbumId"),
                ("ArtistId", _tdColumnField _tdAlbumsTableName "ArtistId"),
                ("Title", _tdColumnField _tdAlbumsTableName "Title")
              ]
      let insertOperation =
            albumsInsertOperation
              & imoRows .~ newAcdcAlbums
              & imoReturningFields .~ returning
      let mutationRequest =
            Data.emptyMutationRequest
              & mrOperations .~ [InsertOperation insertOperation]
              & mrInsertSchema .~ [albumsInsertSchema]
      response <- mutationGuarded mutationRequest

      let expectedResult = MutationOperationResults 3 (Just (expectedInsertedAcdcAlbums albumsStartingId))

      response `mutationResponseShouldBe` MutationResponse [expectedResult]

    for_ _cRelationships $ \_relationshipCapabilities -> do
      usesDataset chinookTemplate $ it "can return rows from an object relationship" $ do
        let rows = take 1 newAcdcAlbums ++ take 1 newApocalypticaAlbums
        let returning =
              Data.mkFieldsMap
                [ ("AlbumId", _tdColumnField _tdAlbumsTableName "AlbumId"),
                  ("Title", _tdColumnField _tdAlbumsTableName "Title"),
                  ( "Artist",
                    ( RelField
                        ( RelationshipField _tdArtistRelationshipName $
                            Data.emptyQuery
                              & qFields
                                ?~ Data.mkFieldsMap
                                  [ ("ArtistId", _tdColumnField _tdArtistsTableName "ArtistId"),
                                    ("Name", _tdColumnField _tdArtistsTableName "Name")
                                  ]
                        )
                    )
                  )
                ]
        let insertOperation =
              albumsInsertOperation
                & imoRows .~ rows
                & imoReturningFields .~ returning
        let mutationRequest =
              Data.emptyMutationRequest
                & mrOperations .~ [InsertOperation insertOperation]
                & mrInsertSchema .~ [albumsInsertSchema]
                & mrTableRelationships .~ [Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships]

        response <- mutationGuarded mutationRequest

        let joinInArtist (album :: HashMap FieldName FieldValue) =
              let artist = (album ^? Data.field "ArtistId" . Data._ColumnFieldNumber) >>= \artistId -> _tdArtistsRowsById ^? ix artistId
                  artistPropVal = maybeToList artist
               in Data.insertField "Artist" (mkSubqueryResponse artistPropVal) album
        let removeArtistId = Data.deleteField "ArtistId"

        let expectedRows =
              take 1 (expectedInsertedAcdcAlbums albumsStartingId)
                ++ take 1 (expectedInsertedApocalypticaAlbums (albumsStartingId + 1))
                & fmap (joinInArtist >> removeArtistId)

        let expectedResult = MutationOperationResults 2 (Just expectedRows)

        response `mutationResponseShouldBe` MutationResponse [expectedResult]
  where
    mkSubqueryResponse :: [HashMap FieldName FieldValue] -> FieldValue
    mkSubqueryResponse rows =
      mkRelationshipFieldValue $ QueryResponse (Just rows) Nothing

    artistsInsertOperation :: InsertMutationOperation
    artistsInsertOperation = InsertMutationOperation _tdArtistsTableName [] Nothing mempty

    albumsInsertOperation :: InsertMutationOperation
    albumsInsertOperation = InsertMutationOperation _tdAlbumsTableName [] Nothing mempty

    artistsInsertSchema :: TableInsertSchema
    artistsInsertSchema =
      TableInsertSchema _tdArtistsTableName $
        HashMap.fromList
          [ (FieldName "Name", ColumnInsert (_tdColumnInsertSchema _tdArtistsTableName "Name"))
          ]

    albumsInsertSchema :: TableInsertSchema
    albumsInsertSchema =
      TableInsertSchema _tdAlbumsTableName $
        HashMap.fromList
          [ (FieldName "ArtistId", ColumnInsert (_tdColumnInsertSchema _tdAlbumsTableName "ArtistId")),
            (FieldName "Title", ColumnInsert (_tdColumnInsertSchema _tdAlbumsTableName "Title"))
          ]

    albumsStartingId :: Integer
    albumsStartingId = 348

    newArtists :: [RowObject]
    newArtists =
      [ RowObject . Data.mkFieldsMap $
          [ ("Name", mkColumnInsertFieldValue $ J.String "Taylor Swift")
          ],
        RowObject . Data.mkFieldsMap $
          [ ("Name", mkColumnInsertFieldValue $ J.String "John Williams")
          ],
        RowObject . Data.mkFieldsMap $
          [ ("Name", mkColumnInsertFieldValue $ J.String "Nightwish")
          ],
        RowObject . Data.mkFieldsMap $
          [ ("Name", mkColumnInsertFieldValue $ J.String "Gareth Emery")
          ]
      ]

    newAcdcAlbums :: [RowObject]
    newAcdcAlbums =
      [ RowObject . Data.mkFieldsMap $
          [ ("ArtistId", mkColumnInsertFieldValue $ J.Number 1),
            ("Title", mkColumnInsertFieldValue $ J.String "Dirty Deeds Done Dirt Cheap")
          ],
        RowObject . Data.mkFieldsMap $
          [ ("ArtistId", mkColumnInsertFieldValue $ J.Number 1),
            ("Title", mkColumnInsertFieldValue $ J.String "Highway to Hell")
          ],
        RowObject . Data.mkFieldsMap $
          [ ("ArtistId", mkColumnInsertFieldValue $ J.Number 1),
            ("Title", mkColumnInsertFieldValue $ J.String "Ballbreaker")
          ]
      ]

    expectedInsertedAcdcAlbums :: Integer -> [HashMap FieldName FieldValue]
    expectedInsertedAcdcAlbums startingId =
      Data.insertAutoIncPk "AlbumId" startingId $
        [ Data.mkFieldsMap
            [ ("ArtistId", mkColumnFieldValue $ J.Number 1),
              ("Title", mkColumnFieldValue $ J.String "Dirty Deeds Done Dirt Cheap")
            ],
          Data.mkFieldsMap
            [ ("ArtistId", mkColumnFieldValue $ J.Number 1),
              ("Title", mkColumnFieldValue $ J.String "Highway to Hell")
            ],
          Data.mkFieldsMap
            [ ("ArtistId", mkColumnFieldValue $ J.Number 1),
              ("Title", mkColumnFieldValue $ J.String "Ballbreaker")
            ]
        ]

    newApocalypticaAlbums :: [RowObject]
    newApocalypticaAlbums =
      [ RowObject . Data.mkFieldsMap $
          [ ("ArtistId", mkColumnInsertFieldValue $ J.Number 2),
            ("Title", mkColumnInsertFieldValue $ J.String "Cult")
          ]
      ]

    expectedInsertedApocalypticaAlbums :: Integer -> [HashMap FieldName FieldValue]
    expectedInsertedApocalypticaAlbums startingId =
      Data.insertAutoIncPk "AlbumId" startingId $
        [ Data.mkFieldsMap
            [ ("ArtistId", mkColumnFieldValue $ J.Number 2),
              ("Title", mkColumnFieldValue $ J.String "Cult")
            ]
        ]
