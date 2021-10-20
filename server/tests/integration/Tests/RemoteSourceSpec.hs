-- | Tests for DB-to-DB joins.
module Tests.RemoteSourceSpec
  ( spec,
  )
where

import Backend.Postgres (withPostgres)
import Control.Exception (throwIO)
import Data.Aeson (ToJSON (..), Value (..), toJSON)
import Data.HashMap.Strict qualified as HM
import Data.HashMap.Strict.InsOrd.Extended qualified as OM
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.NonEmpty (mkNonEmptyTextUnsafe)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
  ( BoolExp (..),
    ColExp (..),
    GBoolExp (BoolFld),
    gBoolExpTrue,
  )
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.Common
  ( FieldName (..),
    RelType (ArrRel, ObjRel),
    SourceName (..),
  )
import Hasura.RQL.Types.Metadata
  ( Metadata (..),
    RemoteRelationshipMetadata (..),
    SourceMetadata (..),
    TableMetadata (..),
    emptyMetadata,
    mkTableMeta,
  )
import Hasura.RQL.Types.Permission
  ( PermColSpec (..),
    PermDef (..),
    SelPerm (..),
  )
import Hasura.RQL.Types.RemoteRelationship
  ( RemoteRelationshipDef (..),
    RemoteRelationshipName (..),
    RemoteSourceRelationshipDef (..),
  )
import Hasura.RQL.Types.Table (emptyTableConfig)
import Hasura.SQL.AnyBackend (mkAnyBackend)
import Hasura.SQL.Backend (BackendType (..), PostgresKind (..))
import Hasura.Session (mkRoleName)
import Hasura.Test.App (withHasuraTestApp)
import Hasura.Test.Requests (postgresRunSql, replaceMetadata, v1graphql)
import Network.Wai (Application)
import Test.Hspec (Spec, SpecWith, aroundAll, describe, it, xit)
import Test.Hspec.Wai (getState, shouldRespondWith)
import Tests.RemoteSourceSql
  ( RemoteSourceSql (..),
    albums,
    artist_id,
    artist_id_null,
    artist_name,
    artists,
    fav_album,
    id_,
    id_null,
    name,
    remoteSourceSql_postgres,
    title,
  )
import Text.Shakespeare.Text (st)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  aroundAll withMetadata $ do
    setup remoteSourceSql_postgres
    remoteSourcesSpec

withMetadata ::
  ((SourceMetadata ('Postgres 'Vanilla), Application) -> IO b) -> IO b
withMetadata action = do
  result <- fmap join . withPostgres $ \metadataDbUrl _ ->
    withPostgres $ \_ initialSourceMetadata ->
      withHasuraTestApp metadataDbUrl $ \hasuraApp ->
        liftIO $ action (initialSourceMetadata, hasuraApp)
  onLeft result throwIO

--------------------------------------------------------------------------------
-- Test setup

setup ::
  (Backend b) => RemoteSourceSql b -> SpecWith (SourceMetadata b, Application)
setup sqlConfig = do
  describe "DB-to-DB tests setup" $ do
    it "Setup initial metadata for runSQL" $ do
      initialSourceMetadata <- getState
      let metadata =
            emptyMetadata
              { _metaSources =
                  OM.fromList
                    [ ( _smName initialSourceMetadata,
                        mkAnyBackend initialSourceMetadata
                      )
                    ]
              }
          expectedResponse =
            "{\"is_consistent\":true,\"inconsistent_objects\":[]}"
      replaceMetadata metadata `shouldRespondWith` expectedResponse

    it "Populate database via runSQL" $ do
      let expectedResponse =
            "{\"result_type\":\"CommandOk\",\"result\":null}"
      -- TODO: Oops, this isn't backend-agnostic
      postgresRunSql (setupSql sqlConfig) `shouldRespondWith` expectedResponse

    it "Upload test metadata" $ do
      initialSourceMetadata <- getState
      let metadata =
            remoteSourceJoinsTestMetadata sqlConfig initialSourceMetadata
          expectedResponse =
            "{\"is_consistent\":true,\"inconsistent_objects\":[]}"
      replaceMetadata metadata `shouldRespondWith` expectedResponse

--------------------------------------------------------------------------------
-- Metadata

-- Currently defines two sources pointing to the same database
remoteSourceJoinsTestMetadata ::
  forall b. (Backend b) => RemoteSourceSql b -> SourceMetadata b -> Metadata
remoteSourceJoinsTestMetadata sqlConfig origSource =
  emptyMetadata
    { _metaSources =
        let connConfig = _smConfiguration origSource
            withName = _smName &&& (mkAnyBackend @b)
         in OM.fromList $
              map
                withName
                [ origSource,
                  albumsSource sqlConfig connConfig,
                  artistsSource sqlConfig connConfig
                ]
    }

--------------------------------------------------------------------------------
-- Albums source metadata

albumsSourceName :: SourceName
albumsSourceName = SNName $ mkNonEmptyTextUnsafe "albumsSource"

albumsSource ::
  Backend b =>
  RemoteSourceSql b ->
  SourceConnConfiguration b ->
  SourceMetadata b
albumsSource sqlConfig connConfig =
  SourceMetadata
    { _smName = albumsSourceName,
      _smTables =
        let withName = _tmTable &&& id
         in OM.fromList $ map withName [albumsTable sqlConfig],
      _smFunctions = mempty,
      _smConfiguration = connConfig,
      _smQueryTags = Nothing
    }

albumsTable :: forall b. (Backend b) => RemoteSourceSql b -> TableMetadata b
albumsTable sqlConfig =
  baseTable
    { _tmRemoteRelationships =
        let withName = _rrmName &&& id
         in OM.fromList $
              map
                withName
                [ albumsRemoteRel,
                  artistsRemoteRel,
                  artistsNullRemoteRel,
                  favouriteOfRemoteRel,
                  ambiguousArtistRemoteRel,
                  artistsIncompatibleColumnsRemoteRel
                ],
      _tmSelectPermissions =
        let withName = _pdRole &&& id
         in OM.fromList $
              map
                withName
                [ albumsObjRelUserSelectPermission,
                  albumsOnlyTitleSelectPermission,
                  albumsArrRelUserSelectPermission
                ]
    }
  where
    baseTable :: TableMetadata b
    baseTable = mkTableMeta (albumsTableName sqlConfig) False emptyTableConfig

    -- A self-relationship
    albumsRemoteRel :: RemoteRelationshipMetadata
    albumsRemoteRel =
      RemoteRelationshipMetadata
        { _rrmName = RemoteRelationshipName $ mkNonEmptyTextUnsafe albums,
          _rrmDefinition =
            RemoteSourceRelDef $
              RemoteSourceRelationshipDef
                { _rsrRelationshipType = ObjRel,
                  _rsrFieldMapping =
                    HM.singleton (FieldName id_) (FieldName id_),
                  _rsrSource = albumsSourceName,
                  _rsrTable = toJSON (albumsTableName sqlConfig)
                }
        }

    -- A standard object relationship
    artistsRemoteRel :: RemoteRelationshipMetadata
    artistsRemoteRel =
      RemoteRelationshipMetadata
        { _rrmName = RemoteRelationshipName $ mkNonEmptyTextUnsafe artists,
          _rrmDefinition =
            RemoteSourceRelDef $
              RemoteSourceRelationshipDef
                { _rsrRelationshipType = ObjRel,
                  _rsrFieldMapping =
                    HM.singleton (FieldName artist_id) (FieldName id_),
                  _rsrSource = artistsSourceName,
                  _rsrTable = toJSON (artistsTableName sqlConfig)
                }
        }

    -- An object relationship where some remote join columns are null
    artistsNullRemoteRel :: RemoteRelationshipMetadata
    artistsNullRemoteRel =
      RemoteRelationshipMetadata
        { _rrmName =
            RemoteRelationshipName $ mkNonEmptyTextUnsafe "artists_null",
          _rrmDefinition =
            RemoteSourceRelDef $
              RemoteSourceRelationshipDef
                { _rsrRelationshipType = ObjRel,
                  _rsrFieldMapping =
                    HM.fromList
                      [ (FieldName artist_id, FieldName id_),
                        (FieldName artist_id, FieldName id_null)
                      ],
                  _rsrSource = artistsSourceName,
                  _rsrTable = toJSON (artistsTableName sqlConfig)
                }
        }

    -- An object relationship that joins over two fields
    favouriteOfRemoteRel :: RemoteRelationshipMetadata
    favouriteOfRemoteRel =
      RemoteRelationshipMetadata
        { _rrmName =
            RemoteRelationshipName $ mkNonEmptyTextUnsafe "favouriteOf",
          _rrmDefinition =
            RemoteSourceRelDef $
              RemoteSourceRelationshipDef
                { _rsrRelationshipType = ObjRel,
                  _rsrFieldMapping =
                    HM.fromList
                      [ (FieldName artist_id, FieldName id_),
                        (FieldName title, FieldName fav_album)
                      ],
                  _rsrSource = artistsSourceName,
                  _rsrTable = toJSON (artistsTableName sqlConfig)
                }
        }

    -- A join using this object relationship yields more than one "remote" row
    -- for some "local" rows.
    ambiguousArtistRemoteRel :: RemoteRelationshipMetadata
    ambiguousArtistRemoteRel =
      RemoteRelationshipMetadata
        { _rrmName =
            RemoteRelationshipName $ mkNonEmptyTextUnsafe "ambiguous_artist",
          _rrmDefinition =
            RemoteSourceRelDef $
              RemoteSourceRelationshipDef
                { _rsrRelationshipType = ObjRel,
                  _rsrFieldMapping =
                    HM.fromList
                      [ (FieldName artist_name, FieldName name)
                      ],
                  _rsrSource = artistsSourceName,
                  _rsrTable = toJSON (artistsTableName sqlConfig)
                }
        }

    -- A permissive permission for role "objRel-user"
    albumsObjRelUserSelectPermission :: PermDef (SelPerm b)
    albumsObjRelUserSelectPermission =
      PermDef
        { _pdRole = fromJust $ mkRoleName "objRel-user",
          _pdPermission =
            SelPerm
              { spColumns = PCStar,
                spFilter = BoolExp gBoolExpTrue,
                spLimit = Nothing,
                spAllowAggregations = True,
                spComputedFields = []
              },
          _pdComment = Nothing
        }

    -- A permission denying access to the "artists" remote source relationship
    albumsOnlyTitleSelectPermission :: PermDef (SelPerm b)
    albumsOnlyTitleSelectPermission =
      PermDef
        { _pdRole = fromJust $ mkRoleName "only-albums-artist_id",
          _pdPermission =
            SelPerm
              { spColumns = PCCols [albumsArtistIdColumn sqlConfig],
                spFilter = BoolExp gBoolExpTrue,
                spLimit = Nothing,
                spAllowAggregations = True,
                spComputedFields = []
              },
          _pdComment = Nothing
        }

    -- This object relationship attempts join incompatible columns. We expect a
    -- runtime error.
    artistsIncompatibleColumnsRemoteRel :: RemoteRelationshipMetadata
    artistsIncompatibleColumnsRemoteRel =
      RemoteRelationshipMetadata
        { _rrmName =
            RemoteRelationshipName $
              mkNonEmptyTextUnsafe "artists_incompatible_columns",
          _rrmDefinition =
            RemoteSourceRelDef $
              RemoteSourceRelationshipDef
                { _rsrRelationshipType = ObjRel,
                  _rsrFieldMapping =
                    HM.fromList
                      [ (FieldName id_, FieldName name)
                      ],
                  _rsrSource = artistsSourceName,
                  _rsrTable = toJSON (artistsTableName sqlConfig)
                }
        }

    -- A select permission for role "arrRel-user" restricting access to (1)
    -- rows matching the user's ID and (2) the "artist_id" column, while
    -- denying access to aggregations.
    albumsArrRelUserSelectPermission :: PermDef (SelPerm b)
    albumsArrRelUserSelectPermission =
      PermDef
        { _pdRole = fromJust $ mkRoleName "arrRel-user",
          _pdPermission =
            SelPerm
              { spColumns = PCCols [albumsArtistIdColumn sqlConfig],
                spFilter =
                  BoolExp $
                    BoolFld $
                      ColExp
                        { ceCol = FieldName artist_id,
                          ceVal =
                            Object $ HM.singleton "_eq" "X-Hasura-User-Id"
                        },
                spLimit = Nothing,
                spAllowAggregations = False,
                spComputedFields = []
              },
          _pdComment = Nothing
        }

--------------------------------------------------------------------------------
-- Artists source metadata

artistsSourceName :: SourceName
artistsSourceName = SNName $ mkNonEmptyTextUnsafe "artistsSource"

artistsSource ::
  (Backend b) =>
  RemoteSourceSql b ->
  SourceConnConfiguration b ->
  SourceMetadata b
artistsSource sqlConfig connConfig =
  SourceMetadata
    { _smName = artistsSourceName,
      _smTables =
        let withName = _tmTable &&& id
         in OM.fromList $ map withName [artistsTable sqlConfig],
      _smFunctions = mempty,
      _smConfiguration = connConfig,
      _smQueryTags = Nothing
    }

artistsTable :: forall b. (Backend b) => RemoteSourceSql b -> TableMetadata b
artistsTable sqlConfig =
  baseTable
    { _tmRemoteRelationships =
        let withName = _rrmName &&& id
         in OM.fromList $
              map
                withName
                [ albumsRemoteRel,
                  albumsNullRemoteRel,
                  favAlbumsRemoteRel
                ],
      _tmSelectPermissions =
        let withName = _pdRole &&& id
         in OM.fromList $
              map
                withName
                [ artistsObjRelUserSelectPermission,
                  artistsArrRelUserSelectPermission,
                  artistsOnlyIdSelectPermission
                ]
    }
  where
    baseTable :: TableMetadata b
    baseTable = mkTableMeta (artistsTableName sqlConfig) False emptyTableConfig

    -- A standard array relationship
    albumsRemoteRel :: RemoteRelationshipMetadata
    albumsRemoteRel =
      RemoteRelationshipMetadata
        { _rrmName = RemoteRelationshipName $ mkNonEmptyTextUnsafe albums,
          _rrmDefinition =
            RemoteSourceRelDef $
              RemoteSourceRelationshipDef
                { _rsrRelationshipType = ArrRel,
                  _rsrFieldMapping =
                    HM.singleton (FieldName id_) (FieldName artist_id),
                  _rsrSource = albumsSourceName,
                  _rsrTable = toJSON (albumsTableName sqlConfig)
                }
        }

    -- An array relationship where some remote join columns are null
    albumsNullRemoteRel :: RemoteRelationshipMetadata
    albumsNullRemoteRel =
      RemoteRelationshipMetadata
        { _rrmName =
            RemoteRelationshipName $ mkNonEmptyTextUnsafe "albums_null",
          _rrmDefinition =
            RemoteSourceRelDef $
              RemoteSourceRelationshipDef
                { _rsrRelationshipType = ArrRel,
                  _rsrFieldMapping =
                    HM.fromList
                      [ (FieldName id_, FieldName artist_id),
                        (FieldName id_, FieldName artist_id_null)
                      ],
                  _rsrSource = albumsSourceName,
                  _rsrTable = toJSON (albumsTableName sqlConfig)
                }
        }

    -- An array relationship that joins over two fields
    favAlbumsRemoteRel :: RemoteRelationshipMetadata
    favAlbumsRemoteRel =
      RemoteRelationshipMetadata
        { _rrmName = RemoteRelationshipName $ mkNonEmptyTextUnsafe "favAlbums",
          _rrmDefinition =
            RemoteSourceRelDef $
              RemoteSourceRelationshipDef
                { _rsrRelationshipType = ArrRel,
                  _rsrFieldMapping =
                    HM.fromList
                      [ (FieldName id_, FieldName artist_id),
                        (FieldName fav_album, FieldName title)
                      ],
                  _rsrSource = albumsSourceName,
                  _rsrTable = toJSON (albumsTableName sqlConfig)
                }
        }

    -- A select permission for role "objRel-user" restricting access to (1)
    -- rows matching the user's ID and (2) the "id" column, while denying
    -- access to aggregations.
    artistsObjRelUserSelectPermission :: PermDef (SelPerm b)
    artistsObjRelUserSelectPermission =
      PermDef
        { _pdRole = fromJust $ mkRoleName "objRel-user",
          _pdPermission =
            SelPerm
              { spColumns = PCCols [artistsIdColumn sqlConfig],
                spFilter =
                  BoolExp $
                    BoolFld $
                      ColExp
                        { ceCol = FieldName id_,
                          ceVal =
                            Object $ HM.singleton "_eq" "X-Hasura-User-Id"
                        },
                spLimit = Nothing,
                spAllowAggregations = False,
                spComputedFields = []
              },
          _pdComment = Nothing
        }

    -- A permissive permission for role "arrRel-user"
    artistsArrRelUserSelectPermission :: PermDef (SelPerm b)
    artistsArrRelUserSelectPermission =
      PermDef
        { _pdRole = fromJust $ mkRoleName "arrRel-user",
          _pdPermission =
            SelPerm
              { spColumns = PCStar,
                spFilter = BoolExp gBoolExpTrue,
                spLimit = Nothing,
                spAllowAggregations = True,
                spComputedFields = []
              },
          _pdComment = Nothing
        }

    -- A permission denying access to the "albums" remote source relationship
    artistsOnlyIdSelectPermission :: PermDef (SelPerm b)
    artistsOnlyIdSelectPermission =
      PermDef
        { _pdRole = fromJust $ mkRoleName "only-artists-id",
          _pdPermission =
            SelPerm
              { spColumns = PCCols [artistsIdColumn sqlConfig],
                spFilter = BoolExp gBoolExpTrue,
                spLimit = Nothing,
                spAllowAggregations = True,
                spComputedFields = []
              },
          _pdComment = Nothing
        }

--------------------------------------------------------------------------------
-- Tests

-- | These tests are a work-in-progress. They may not properly exercise the
-- functionality they intend to test.
remoteSourcesSpec :: SpecWith (SourceMetadata b, Application)
remoteSourcesSpec =
  describe "DB-to-DB queries" $ do
    it "Self-join" $ do
      let query =
            T.unpack
              [st|
              query {
                #{albums} {
                  #{albums} {
                    #{albums} {
                      #{albums} {
                        #{title}
                      }
                    }
                  }
                }
              }
            |]
          expectedResponse =
            "{\"data\":{\"albums\":[{\"albums\":{\"albums\":{\"albums\":{\"title\":\"Album 1A\"}}}},{\"albums\":{\"albums\":{\"albums\":{\"title\":\"Album 1B\"}}}},{\"albums\":{\"albums\":{\"albums\":{\"title\":\"Album 2A\"}}}},{\"albums\":{\"albums\":{\"albums\":{\"title\":\"Album 2B\"}}}}]}}"
      v1graphql [] query `shouldRespondWith` expectedResponse

    it "Mutually recursive joins (1)" $ do
      let query =
            T.unpack
              [st|
              query {
                #{albums} {
                  #{title}
                  #{artists} {
                    #{id_}
                    #{albums} {
                      #{title}
                    }
                  }
                }
              }
              |]
          expectedResponse =
            "{\"data\":{\"albums\":[{\"title\":\"Album 1A\",\"artists\":{\"id\":1,\"albums\":[{\"title\":\"Album 1A\"},{\"title\":\"Album 1B\"}]}},{\"title\":\"Album 1B\",\"artists\":{\"id\":1,\"albums\":[{\"title\":\"Album 1A\"},{\"title\":\"Album 1B\"}]}},{\"title\":\"Album 2A\",\"artists\":{\"id\":2,\"albums\":[{\"title\":\"Album 2A\"},{\"title\":\"Album 2B\"}]}},{\"title\":\"Album 2B\",\"artists\":{\"id\":2,\"albums\":[{\"title\":\"Album 2A\"},{\"title\":\"Album 2B\"}]}}]}}"
      v1graphql [] query `shouldRespondWith` expectedResponse

    it "Mutually recursive joins (2)" $ do
      let query =
            T.unpack
              [st|
              query {
                #{artists} {
                  #{id_}
                  #{albums} {
                    #{title}
                    #{artists} {
                      #{id_}
                    }
                  }
                }
              }
            |]
          expectedResponse =
            "{\"data\":{\"artists\":[{\"id\":1,\"albums\":[{\"title\":\"Album 1A\",\"artists\":{\"id\":1}},{\"title\":\"Album 1B\",\"artists\":{\"id\":1}}]},{\"id\":2,\"albums\":[{\"title\":\"Album 2A\",\"artists\":{\"id\":2}},{\"title\":\"Album 2B\",\"artists\":{\"id\":2}}]}]}}"
      v1graphql [] query `shouldRespondWith` expectedResponse

    it "Join over multiple fields, array relationship" $ do
      let query =
            T.unpack
              [st|
              query {
                #{artists} {
                  #{id_}
                  favAlbums {
                    #{title}
                  }
                }
              }
              |]
          expectedResponse =
            "{\"data\":{\"artists\":[{\"id\":1,\"favAlbums\":[{\"title\":\"Album 1A\"}]},{\"id\":2,\"favAlbums\":[{\"title\":\"Album 2A\"}]}]}}"
      v1graphql [] query `shouldRespondWith` expectedResponse

    it "Join over multiple fields, object relationship" $ do
      let query =
            T.unpack
              [st|
              query {
                #{albums} {
                  #{title}
                  favouriteOf {
                    #{id_}
                  }
                }
              }
              |]
      v1graphql [] query `shouldRespondWith` "{\"data\":{\"albums\":[{\"title\":\"Album 1A\",\"favouriteOf\":{\"id\":1}},{\"title\":\"Album 1B\",\"favouriteOf\":null},{\"title\":\"Album 2A\",\"favouriteOf\":{\"id\":2}},{\"title\":\"Album 2B\",\"favouriteOf\":null}]}}"

    xit "Object relationship with more than one match" $ do
      -- Marked as pending for the following reason:
      -- One might expect a 'null' response, but currently one of the multiple
      -- matches is returned.
      let query =
            T.unpack
              [st|
              query {
                #{albums} {
                  #{title}
                  ambiguous_artist {
                    #{id_}
                  }
                }
              }
              |]
          expectedResponse =
            "{\"data\":{\"albums\":[{\"title\":\"Album 1A\",\"ambiguous_artist\":null},{\"title\":\"Album 1B\",\"ambiguous_artist\":null},{\"title\":\"Album 2A\",\"ambiguous_artist\":null},{\"title\":\"Album 2B\",\"ambiguous_artist\":null}]}}"
      v1graphql [] query `shouldRespondWith` expectedResponse

    -- Permissions

    it "Object relationships respect the row permissions of the target" $ do
      let query =
            T.unpack
              [st|
              query {
                #{albums} {
                  #{title}
                  #{artists} {
                    #{id_}
                  }
                }
              }
            |]
          expectedResponse = "{\"data\":{\"albums\":[{\"title\":\"Album 1A\",\"artists\":{\"id\":1}},{\"title\":\"Album 1B\",\"artists\":{\"id\":1}},{\"title\":\"Album 2A\",\"artists\":null},{\"title\":\"Album 2B\",\"artists\":null}]}}"
          headers =
            [ ("X-Hasura-Role", "objRel-user"),
              ("X-Hasura-User-Id", "1")
            ]
      v1graphql headers query `shouldRespondWith` expectedResponse

    it "Object relationships respect the column permissions of the target" $ do
      let query =
            T.unpack
              [st|
              query {
                #{albums} {
                  #{artists} {
                    #{name}
                  }
                }
              }
            |]
          expectedResponse = "{\"errors\":[{\"extensions\":{\"path\":\"$.selectionSet.albums.selectionSet.artists.selectionSet.name\",\"code\":\"validation-failed\"},\"message\":\"field \\\"name\\\" not found in type: 'artists'\"}]}"
          headers =
            [ ("X-Hasura-Role", "objRel-user"),
              ("X-Hasura-User-Id", "1")
            ]
      v1graphql headers query `shouldRespondWith` expectedResponse

    -- Note: I'm not sure of the precise conditions under which remote source
    -- relationships should be visible.
    it "Object relationships are not visible without authorization" $ do
      let query =
            T.unpack
              [st|
              query {
                #{albums} {
                  #{artist_id}
                  #{artists} {
                    #{id_}
                  }
                }
              }
            |]
          expectedResponse = "{\"errors\":[{\"extensions\":{\"path\":\"$.selectionSet.albums.selectionSet.artists\",\"code\":\"validation-failed\"},\"message\":\"field \\\"artists\\\" not found in type: 'albums'\"}]}"
          headers = [("X-Hasura-Role", "only-albums-artist_id")]
      v1graphql headers query `shouldRespondWith` expectedResponse

    it "Array relationships respect the row permissions of the target" $ do
      let query =
            T.unpack
              [st|
              query {
                #{artists} {
                  #{id_}
                  #{albums} {
                    #{artist_id}
                  }
                }
              }
            |]
          expectedResponse = "{\"data\":{\"artists\":[{\"id\":1,\"albums\":[{\"artist_id\":1},{\"artist_id\":1}]},{\"id\":2,\"albums\":[]}]}}"
          headers =
            [ ("X-Hasura-Role", "arrRel-user"),
              ("X-Hasura-User-Id", "1")
            ]
      v1graphql headers query `shouldRespondWith` expectedResponse

    it "Array relationships respect the column permissions of the target" $ do
      let query =
            T.unpack
              [st|
              query {
                #{artists} {
                  #{id_}
                  #{albums} {
                    #{title}
                  }
                }
              }
            |]
          expectedResponse = "{\"errors\":[{\"extensions\":{\"path\":\"$.selectionSet.artists.selectionSet.albums.selectionSet.title\",\"code\":\"validation-failed\"},\"message\":\"field \\\"title\\\" not found in type: 'albums'\"}]}"
          headers =
            [ ("X-Hasura-Role", "arrRel-user"),
              ("X-Hasura-User-Id", "1")
            ]
      v1graphql headers query `shouldRespondWith` expectedResponse

    it "Array relationships respect the aggregation permissions of the target" $ do
      let query =
            T.unpack
              [st|
              query {
                #{artists} {
                  #{albums}_aggregate {
                    aggregate {
                      count
                    }
                  }
                }
              }
            |]
          expectedResponse = "{\"errors\":[{\"extensions\":{\"path\":\"$.selectionSet.artists.selectionSet.albums_aggregate\",\"code\":\"validation-failed\"},\"message\":\"field \\\"albums_aggregate\\\" not found in type: 'artists'\"}]}"
          headers =
            [ ("X-Hasura-Role", "arrRel-user"),
              ("X-Hasura-User-Id", "1")
            ]
      v1graphql headers query `shouldRespondWith` expectedResponse

    -- Note: I'm not sure of the precise conditions under which remote source
    -- relationships should be visible.
    it "Array relationships are not visible without authorization" $ do
      let query =
            T.unpack
              [st|
              query {
                #{artists} {
                  #{id_}
                  #{albums} {
                    #{id_}
                  }
                }
              }
            |]
          expectedResponse = "{\"errors\":[{\"extensions\":{\"path\":\"$.selectionSet.artists.selectionSet.albums\",\"code\":\"validation-failed\"},\"message\":\"field \\\"albums\\\" not found in type: 'artists'\"}]}"
          headers = [("X-Hasura-Role", "only-artists-id")]
      v1graphql headers query `shouldRespondWith` expectedResponse

    -- Schema generation

    it "Object remote sources should not be present in bool_exp" $ do
      let query =
            T.unpack
              [st|
              query {
                #{albums}(where: {#{artists}: {#{id_}: {_gt: 1}}}) {
                  #{title}
                }
              }
            |]
          expectedResponse = "{\"errors\":[{\"extensions\":{\"path\":\"$.selectionSet.albums.args.where.artists\",\"code\":\"validation-failed\"},\"message\":\"field \\\"artists\\\" not found in type: 'albums_bool_exp'\"}]}"
      v1graphql [] query `shouldRespondWith` expectedResponse

    it "Object remote sources should not be present in order_by" $ do
      let query =
            T.unpack
              [st|
              query {
                #{albums}(order_by: {#{artists}: {#{id_}: asc}}) {
                  #{title}
                }
              }
            |]
          expectedResponse = "{\"errors\":[{\"extensions\":{\"path\":\"$.selectionSet.albums.args.order_by[0].artists\",\"code\":\"validation-failed\"},\"message\":\"field \\\"artists\\\" not found in type: 'albums_order_by'\"}]}"
      v1graphql [] query `shouldRespondWith` expectedResponse

    it "Array remote sources should not be present in bool_exp" $ do
      let query =
            T.unpack
              [st|
              query {
                #{artists}(where: {#{albums}: {#{id_}: {_gt: 1}}}) {
                  #{name}
                }
              }
            |]
          expectedResponse = "{\"errors\":[{\"extensions\":{\"path\":\"$.selectionSet.artists.args.where.albums\",\"code\":\"validation-failed\"},\"message\":\"field \\\"albums\\\" not found in type: 'artists_bool_exp'\"}]}"
      v1graphql [] query `shouldRespondWith` expectedResponse

    it "Array remote sources should not be present in order_by" $ do
      let query =
            T.unpack
              [st|
              query {
                #{artists}(order_by: {#{albums}_aggregate: {count: asc}}) {
                  #{name}
                }
              }
            |]
          expectedResponse = "{\"errors\":[{\"extensions\":{\"path\":\"$.selectionSet.artists.args.order_by[0].albums_aggregate\",\"code\":\"validation-failed\"},\"message\":\"field \\\"albums_aggregate\\\" not found in type: 'artists_order_by'\"}]}"
      v1graphql [] query `shouldRespondWith` expectedResponse

    -- Null values

    it "An object relationship returning a row with null for any join column resolves to null" $ do
      let query =
            T.unpack
              [st|
              query {
                #{albums} {
                  artists_null {
                    #{name}
                  }
                }
              }
              |]
          expectedResponse = "{\"data\":{\"albums\":[{\"artists_null\":{\"name\":\"Author\"}},{\"artists_null\":{\"name\":\"Author\"}},{\"artists_null\":null},{\"artists_null\":null}]}}"
      v1graphql [] query `shouldRespondWith` expectedResponse

    xit "An array relationship with rows with null for any join column excludes those rows" $ do
      -- Marked as pending for the following reason:
      -- I'm unsure whether this result satisfies our acceptance criteria.
      -- Should we return '[]' if any join column of /any/ row is null?
      let query =
            T.unpack
              [st|
              query {
                #{artists} {
                  albums_null {
                    #{title}
                  }
                }
              }
              |]
          expectedResponse = "{\"data\":{\"artists\":[{\"albums_null\":[{\"title\":\"Album 1A\"}]},{\"albums_null\":[{\"title\":\"Album 2A\"}]}]}}"
      v1graphql [] query `shouldRespondWith` expectedResponse

    xit "Incompatible join columns (different data types)" $ do
      -- Marked as pending for the following reason:
      -- Not sure what the expected behaviour should be when a remote source
      -- relationship attempts to join incompatible columns. At the time of
      -- writing, we return a response with `null` values for the remote source
      -- relationship, though one might expect that we prevent such
      -- relationships from being registered to the metadata in the first
      -- place.
      let query =
            T.unpack
              [st|
              query {
                #{albums} {
                  artists_incompatible_columns {
                    #{name}
                  }
                }
              }
              |]
          expectedResponse = "{\"data\":{\"albums\":[{\"artists_incompatible_columns\":null},{\"artists_incompatible_columns\":null},{\"artists_incompatible_columns\":null},{\"artists_incompatible_columns\":null}]}}"
      v1graphql [] query `shouldRespondWith` expectedResponse

    -- Array relationship functionality

    it "Array relationships work with 'distinct_on'" $ do
      let query =
            T.unpack
              [st|
              query {
                #{artists} {
                  albums(distinct_on: #{artist_id}) {
                    #{artist_id}
                    #{title}
                  }
                }
              }
              |]
          expectedResponse = "{\"data\":{\"artists\":[{\"albums\":[{\"artist_id\":1,\"title\":\"Album 1A\"}]},{\"albums\":[{\"artist_id\":2,\"title\":\"Album 2A\"}]}]}}"
      v1graphql [] query `shouldRespondWith` expectedResponse

    it "Array relationships work with 'where'" $ do
      let query =
            T.unpack
              [st|
              query {
                #{artists} {
                  albums(where: {#{title}: {_like: "%1%"}}) {
                    #{title}
                  }
                }
              }
              |]
          expectedResponse = "{\"data\":{\"artists\":[{\"albums\":[{\"title\":\"Album 1A\"},{\"title\":\"Album 1B\"}]},{\"albums\":[]}]}}"
      v1graphql [] query `shouldRespondWith` expectedResponse

    it "Array relationships work with 'limit'" $ do
      let query =
            T.unpack
              [st|
              query {
                #{artists} {
                  albums(limit: 1) {
                    #{title}
                  }
                }
              }
              |]
          expectedResponse = "{\"data\":{\"artists\":[{\"albums\":[{\"title\":\"Album 1A\"}]},{\"albums\":[{\"title\":\"Album 2A\"}]}]}}"
      v1graphql [] query `shouldRespondWith` expectedResponse

    it "Array relationships work with 'offset'" $ do
      let query =
            T.unpack
              [st|
              query {
                #{artists} {
                  albums(offset: 1) {
                    #{title}
                  }
                }
              }
              |]
          expectedResponse = "{\"data\":{\"artists\":[{\"albums\":[{\"title\":\"Album 1B\"}]},{\"albums\":[{\"title\":\"Album 2B\"}]}]}}"
      v1graphql [] query `shouldRespondWith` expectedResponse

    it "Array relationships work with 'order_by'" $ do
      let query =
            T.unpack
              [st|
              query {
                #{artists} {
                  albums(order_by: {#{id_}: desc}) {
                    #{id_}
                    #{title}
                  }
                }
              }
              |]
          expectedResponse = "{\"data\":{\"artists\":[{\"albums\":[{\"id\":2,\"title\":\"Album 1B\"},{\"id\":1,\"title\":\"Album 1A\"}]},{\"albums\":[{\"id\":4,\"title\":\"Album 2B\"},{\"id\":3,\"title\":\"Album 2A\"}]}]}}"
      v1graphql [] query `shouldRespondWith` expectedResponse
