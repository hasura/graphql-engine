module Test.QuerySpec.RelationshipsSpec (spec) where

import Autodocodec.Extended (ValueWrapper (..))
import Control.Lens (ix, (^?))
import Data.Aeson (Object, Value (..))
import Data.Aeson qualified as J
import Data.Aeson.Lens (_Number)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Vector qualified as Vector
import Hasura.Backends.DataConnector.API
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Data qualified as Data
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Prelude

spec :: Client IO (NamedRoutes Routes) -> Config -> Spec
spec api config = describe "Relationship Queries" $ do
  it "perform a many to one query by joining artist to albums" $ do
    let query = albumsWithArtistQuery id
    receivedAlbums <- fmap (Data.sortBy "id" . getQueryResponse) $ (api // _query) config query

    let joinInArtist (album :: Object) =
          let artist = (album ^? ix "artist_id" . _Number) >>= \artistId -> Data.artistsAsJsonById ^? ix artistId
              artistPropVal = maybe J.Null (Array . Vector.singleton . Object) artist
           in HashMap.insert "artist" artistPropVal album
    let removeArtistId = HashMap.delete "artist_id"

    let expectedAlbums = (removeArtistId . joinInArtist) <$> Data.albumsAsJson
    receivedAlbums `shouldBe` expectedAlbums

  it "perform a one to many query by joining albums to artists" $ do
    let query = artistsWithAlbumsQuery id
    receivedArtists <- fmap (Data.sortBy "id" . getQueryResponse) $ (api // _query) config query

    let joinInAlbums (artist :: Object) =
          let artistId = artist ^? ix "id" . _Number
              albums = maybe [] (\artistId' -> filter (\album -> album ^? ix "artist_id" . _Number == Just artistId') Data.albumsAsJson) artistId
              albums' = Object . HashMap.delete "artist_id" <$> albums
           in HashMap.insert "albums" (Array . Vector.fromList $ albums') artist

    let expectedAlbums = joinInAlbums <$> Data.artistsAsJson
    receivedArtists `shouldBe` expectedAlbums

albumsWithArtistQuery :: (Query -> Query) -> Query
albumsWithArtistQuery modifySubquery =
  let joinFieldMapping =
        HashMap.fromList
          [ (PrimaryKey $ ColumnName "artist_id", ForeignKey $ ColumnName "id")
          ]
      artistsSubquery = modifySubquery artistsQuery
      fields =
        HashMap.fromList
          [ ("id", columnField "id"),
            ("title", columnField "title"),
            ("artist", RelationshipField $ RelField joinFieldMapping artistsSubquery)
          ]
   in albumsQuery {fields}

artistsWithAlbumsQuery :: (Query -> Query) -> Query
artistsWithAlbumsQuery modifySubquery =
  let joinFieldMapping =
        HashMap.fromList
          [ (PrimaryKey $ ColumnName "id", ForeignKey $ ColumnName "artist_id")
          ]
      albumFields = HashMap.fromList [("id", columnField "id"), ("title", columnField "title")]
      albumsSort = OrderBy (ColumnName "id") Ascending :| []
      albumsSubquery = modifySubquery (albumsQuery {fields = albumFields, orderBy = Just albumsSort})
      fields =
        HashMap.fromList
          [ ("id", columnField "id"),
            ("name", columnField "name"),
            ("albums", RelationshipField $ RelField joinFieldMapping albumsSubquery)
          ]
   in artistsQuery {fields}

artistsQuery :: Query
artistsQuery =
  let fields = HashMap.fromList [("id", columnField "id"), ("name", columnField "name")]
      tableName = TableName "artists"
   in Query fields tableName Nothing Nothing Nothing Nothing

albumsQuery :: Query
albumsQuery =
  let fields = HashMap.fromList [("id", columnField "id"), ("artist_id", columnField "artist_id"), ("title", columnField "title")]
      tableName = TableName "albums"
   in Query fields tableName Nothing Nothing Nothing Nothing

columnField :: Text -> Field
columnField = ColumnField . ValueWrapper . ColumnName
