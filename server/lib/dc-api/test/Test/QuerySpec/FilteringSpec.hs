module Test.QuerySpec.FilteringSpec (spec) where

import Control.Lens ((&), (.~), (<&>), (?~), (^?))
import Control.Monad (when)
import Data.Aeson (Value (..))
import Data.Foldable (find)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List (sortOn)
import Data.Maybe (isJust, mapMaybe)
import Hasura.Backends.DataConnector.API
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Data (TestData (..))
import Test.Data qualified as Data
import Test.Expectations (jsonShouldBe, rowsShouldBe)
import Test.Hspec (Spec, describe, it)
import Prelude

spec :: TestData -> Client IO (NamedRoutes Routes) -> SourceName -> Config -> Maybe ComparisonCapabilities -> Spec
spec TestData {..} api sourceName config comparisonCapabilities = describe "Filtering in Queries" $ do
  it "can filter using an equality expression" $ do
    let where' = ApplyBinaryComparisonOperator Equal (Data.currentComparisonColumn "AlbumId") (ScalarValue (Number 2))
    let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
    receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

    let expectedAlbums =
          filter ((== Just 2) . (^? Data.field "AlbumId" . Data._ColumnFieldNumber)) _tdAlbumsRows

    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "can filter using an inequality expression" $ do
    let where' = Not (ApplyBinaryComparisonOperator Equal (Data.currentComparisonColumn "AlbumId") (ScalarValue (Number 2)))
    let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
    receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

    let expectedAlbums =
          filter ((/= Just 2) . (^? Data.field "AlbumId" . Data._ColumnFieldNumber)) _tdAlbumsRows

    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "can filter using an in expression" $ do
    let where' = ApplyBinaryArrayComparisonOperator In (Data.currentComparisonColumn "AlbumId") [Number 2, Number 3]
    let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
    receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

    let expectedAlbums =
          filter (flip elem [Just 2, Just 3] . (^? Data.field "AlbumId" . Data._ColumnFieldNumber)) _tdAlbumsRows

    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "can negate an in expression filter using a not expression" $ do
    let where' = Not (ApplyBinaryArrayComparisonOperator In (Data.currentComparisonColumn "AlbumId") [Number 2, Number 3])
    let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
    receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

    let expectedAlbums =
          filter (flip notElem [Just 2, Just 3] . (^? Data.field "AlbumId" . Data._ColumnFieldNumber)) _tdAlbumsRows

    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "can combine filters using an and expression" $ do
    let where1 = ApplyBinaryComparisonOperator Equal (Data.currentComparisonColumn "ArtistId") (ScalarValue (Number 58))
    let where2 = ApplyBinaryComparisonOperator Equal (Data.currentComparisonColumn "Title") (ScalarValue (String "Stormbringer"))
    let where' = And [where1, where2]
    let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
    receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

    let expectedAlbums =
          filter
            ( \album ->
                (album ^? Data.field "ArtistId" . Data._ColumnFieldNumber == Just 58) && (album ^? Data.field "Title" . Data._ColumnFieldString == Just "Stormbringer")
            )
            _tdAlbumsRows

    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "treats an empty and expression as 'true'" $ do
    let where' = And []
    let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
    receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

    Data.responseRows receivedAlbums `rowsShouldBe` _tdAlbumsRows
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "can combine filters using an or expression" $ do
    let where1 = ApplyBinaryComparisonOperator Equal (Data.currentComparisonColumn "AlbumId") (ScalarValue (Number 2))
    let where2 = ApplyBinaryComparisonOperator Equal (Data.currentComparisonColumn "AlbumId") (ScalarValue (Number 3))
    let where' = Or [where1, where2]
    let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
    receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

    let expectedAlbums =
          filter (flip elem [Just 2, Just 3] . (^? Data.field "AlbumId" . Data._ColumnFieldNumber)) _tdAlbumsRows

    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "treats an empty or expression as 'false'" $ do
    let where' = Or []
    let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
    receivedAlbums <- (api // _query) sourceName config query

    Data.responseRows receivedAlbums `rowsShouldBe` []
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "can filter by applying the greater than operator" $ do
    let where' = ApplyBinaryComparisonOperator GreaterThan (Data.currentComparisonColumn "AlbumId") (ScalarValue (Number 300))
    let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
    receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

    let expectedAlbums =
          filter ((> Just 300) . (^? Data.field "AlbumId" . Data._ColumnFieldNumber)) _tdAlbumsRows

    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "can filter by applying the greater than or equal operator" $ do
    let where' = ApplyBinaryComparisonOperator GreaterThanOrEqual (Data.currentComparisonColumn "AlbumId") (ScalarValue (Number 300))
    let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
    receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

    let expectedAlbums =
          filter ((>= Just 300) . (^? Data.field "AlbumId" . Data._ColumnFieldNumber)) _tdAlbumsRows

    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "can filter by applying the less than operator" $ do
    let where' = ApplyBinaryComparisonOperator LessThan (Data.currentComparisonColumn "AlbumId") (ScalarValue (Number 100))
    let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
    receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

    let expectedAlbums =
          filter ((< Just 100) . (^? Data.field "AlbumId" . Data._ColumnFieldNumber)) _tdAlbumsRows

    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "can filter by applying the less than or equal operator" $ do
    let where' = ApplyBinaryComparisonOperator LessThanOrEqual (Data.currentComparisonColumn "AlbumId") (ScalarValue (Number 100))
    let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
    receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

    let expectedAlbums =
          filter ((<= Just 100) . (^? Data.field "AlbumId" . Data._ColumnFieldNumber)) _tdAlbumsRows

    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  it "can filter using a greater than operator with a column comparison" $ do
    let where' = ApplyBinaryComparisonOperator GreaterThan (Data.currentComparisonColumn "AlbumId") (AnotherColumn (Data.currentComparisonColumn "ArtistId"))
    let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
    receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

    let expectedAlbums =
          filter (\album -> (album ^? Data.field "AlbumId" . Data._ColumnFieldNumber) > (album ^? Data.field "ArtistId" . Data._ColumnFieldNumber)) _tdAlbumsRows

    Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
    _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  when (isJust $ comparisonCapabilities >>= _ccSubqueryComparisonCapabilities) $
    describe "Comparisons in unrelated tables" $ do
      describe "can filter with a condition that requires that matching rows exist in another unrelated table" $ do
        describe "compare against a single column" $ do
          it "returns all rows if matching rows exist" $ do
            let where' =
                  Exists (UnrelatedTable _tdEmployeesTableName) $
                    ApplyBinaryComparisonOperator Equal (Data.currentComparisonColumn "EmployeeId") (ScalarValue (Number 1))
            let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
            receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

            let expectedAlbums = _tdAlbumsRows

            Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
            _qrAggregates receivedAlbums `jsonShouldBe` Nothing

          it "returns no rows if matching rows do not exist" $ do
            let where' =
                  Exists (UnrelatedTable _tdEmployeesTableName) $
                    ApplyBinaryComparisonOperator Equal (Data.currentComparisonColumn "EmployeeId") (ScalarValue (Number 0))
            let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
            receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

            Data.responseRows receivedAlbums `rowsShouldBe` []
            _qrAggregates receivedAlbums `jsonShouldBe` Nothing

        describe "compare against multiple columns" $ do
          it "returns all rows if matching rows exist" $ do
            let where' =
                  Exists (UnrelatedTable _tdEmployeesTableName) $
                    And
                      [ ApplyBinaryComparisonOperator Equal (Data.currentComparisonColumn "EmployeeId") (ScalarValue (Number 1)),
                        ApplyBinaryComparisonOperator Equal (Data.currentComparisonColumn "City") (ScalarValue (String "Edmonton"))
                      ]
            let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
            receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

            let expectedAlbums = _tdAlbumsRows

            Data.responseRows receivedAlbums `rowsShouldBe` expectedAlbums
            _qrAggregates receivedAlbums `jsonShouldBe` Nothing

          it "returns no rows if matching rows do not exist" $ do
            let where' =
                  Exists (UnrelatedTable _tdEmployeesTableName) $
                    And
                      [ ApplyBinaryComparisonOperator Equal (Data.currentComparisonColumn "EmployeeId") (ScalarValue (Number 1)),
                        ApplyBinaryComparisonOperator Equal (Data.currentComparisonColumn "City") (ScalarValue (String "Calgary"))
                      ]
            let query = albumsQueryRequest & qrQuery . qWhere ?~ where'
            receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

            Data.responseRows receivedAlbums `rowsShouldBe` []
            _qrAggregates receivedAlbums `jsonShouldBe` Nothing

  when ((comparisonCapabilities >>= _ccSubqueryComparisonCapabilities <&> _ctccSupportsRelations) == Just True) $
    describe "Comparisons in related tables" $ do
      it "can filter by comparing against rows in a related table" $ do
        let where' =
              Exists (RelatedTable _tdArtistRelationshipName) $
                ApplyBinaryComparisonOperator Equal (Data.currentComparisonColumn "Name") (ScalarValue (String "AC/DC"))
        let query =
              albumsQueryRequest
                & qrTableRelationships .~ [Data.onlyKeepRelationships [_tdArtistRelationshipName] _tdAlbumsTableRelationships]
                & qrQuery . qWhere ?~ where'
        receivedAlbums <- Data.sortResponseRowsBy "AlbumId" <$> (api // _query) sourceName config query

        let artistId =
              _tdArtistsRows
                & find (\artist -> (artist ^? Data.field "Name" . Data._ColumnFieldString) == Just "AC/DC")
                >>= (^? Data.field "ArtistId" . Data._ColumnFieldNumber)

        let albums =
              _tdAlbumsRows
                & filter (\album -> (album ^? Data.field "ArtistId" . Data._ColumnFieldNumber) == artistId)
                & sortOn (^? Data.field "AlbumId")

        Data.responseRows receivedAlbums `rowsShouldBe` albums
        _qrAggregates receivedAlbums `jsonShouldBe` Nothing

      it "can filter by comparing against rows in a deeply related table" $ do
        let where' =
              Exists (RelatedTable _tdAlbumsRelationshipName) . Exists (RelatedTable _tdTracksRelationshipName) . Exists (RelatedTable _tdGenreRelationshipName) $
                ApplyBinaryComparisonOperator Equal (Data.currentComparisonColumn "Name") (ScalarValue (String "Metal"))
        let query =
              artistsQueryRequest
                & qrTableRelationships
                  .~ [ Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships,
                       Data.onlyKeepRelationships [_tdTracksRelationshipName] _tdAlbumsTableRelationships,
                       Data.onlyKeepRelationships [_tdGenreRelationshipName] _tdTracksTableRelationships
                     ]
                & qrQuery . qWhere ?~ where'
        receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> (api // _query) sourceName config query

        let genreId =
              _tdGenresRows
                & find (\genre -> (genre ^? Data.field "Name" . Data._ColumnFieldString) == Just "Metal")
                >>= (^? Data.field "GenreId" . Data._ColumnFieldNumber)

        let albumIds =
              _tdTracksRows
                & filter (\track -> (track ^? Data.field "GenreId" . Data._ColumnFieldNumber) == genreId)
                & map (\track -> (track ^? Data.field "AlbumId" . Data._ColumnFieldNumber))
                & HashSet.fromList

        let artists =
              _tdAlbumsRows
                & filter (\album -> HashSet.member (album ^? Data.field "AlbumId" . Data._ColumnFieldNumber) albumIds)
                & mapMaybe (\album -> album ^? Data.field "ArtistId" . Data._ColumnFieldNumber)
                & HashSet.fromList
                & HashSet.toList
                & mapMaybe (\artistId -> HashMap.lookup artistId _tdArtistsRowsById)
                & sortOn (^? Data.field "ArtistId")

        Data.responseRows receivedArtists `rowsShouldBe` artists
        _qrAggregates receivedArtists `jsonShouldBe` Nothing

      it "can filter by comparing against multiple columns in a related table" $ do
        let where' =
              Exists (RelatedTable _tdAlbumsRelationshipName) $
                And
                  [ ApplyBinaryComparisonOperator Equal (Data.currentComparisonColumn "AlbumId") (ScalarValue (Number 1)),
                    ApplyBinaryComparisonOperator Equal (Data.currentComparisonColumn "Title") (ScalarValue (String "Let There Be Rock"))
                  ]
        let query =
              artistsQueryRequest
                & qrTableRelationships .~ [Data.onlyKeepRelationships [_tdAlbumsRelationshipName] _tdArtistsTableRelationships]
                & qrQuery . qWhere ?~ where'
        receivedArtists <- Data.sortResponseRowsBy "ArtistId" <$> (api // _query) sourceName config query

        let albums =
              _tdAlbumsRows
                & filter (\album -> (album ^? Data.field "AlbumId" . Data._ColumnFieldNumber) == Just 1 && (album ^? Data.field "Title" . Data._ColumnFieldString) == Just "Let There Be Rock")

        let artists =
              _tdArtistsRows
                & filter (\artist -> isJust $ find (\album -> (album ^? Data.field "ArtistId" . Data._ColumnFieldNumber) == (artist ^? Data.field "ArtistId" . Data._ColumnFieldNumber)) albums)
                & sortOn (^? Data.field "ArtistId")

        Data.responseRows receivedArtists `rowsShouldBe` artists
        _qrAggregates receivedArtists `jsonShouldBe` Nothing
  where
    artistsQueryRequest :: QueryRequest
    artistsQueryRequest =
      let fields = Data.mkFieldsMap [("ArtistId", Data.columnField "ArtistId"), ("Name", Data.columnField "Name")]
          query = Data.emptyQuery & qFields ?~ fields
       in QueryRequest _tdArtistsTableName [] query

    albumsQueryRequest :: QueryRequest
    albumsQueryRequest =
      let fields = Data.mkFieldsMap [("AlbumId", Data.columnField "AlbumId"), ("ArtistId", Data.columnField "ArtistId"), ("Title", Data.columnField "Title")]
          query = Data.emptyQuery & qFields ?~ fields
       in QueryRequest _tdAlbumsTableName [] query
