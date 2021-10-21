-- | Tests for DB-to-DB joins.
module Tests.RemoteSourceSql
  ( RemoteSourceSql (..),
    remoteSourceSql_postgres,
    -- Database table and column names
    id_,
    artists,
    id_null,
    name,
    fav_album,
    albums,
    title,
    artist_id,
    artist_id_null,
    artist_name,
  )
where

import Hasura.Backends.Postgres.Instances.Types ()
import Hasura.Backends.Postgres.SQL.Types qualified as PG
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.SQL.Backend (BackendType (..), PostgresKind (..))
import Text.Shakespeare.Text (st)

data RemoteSourceSql (b :: BackendType) = RemoteSourceSql
  { setupSql :: Text,
    artistsTableName :: TableName b,
    artistsIdColumn :: Column b,
    albumsTableName :: TableName b,
    albumsArtistIdColumn :: Column b
  }

remoteSourceSql_postgres :: RemoteSourceSql ('Postgres 'Vanilla)
remoteSourceSql_postgres =
  RemoteSourceSql
    { setupSql = setupSql_postgres,
      artistsTableName =
        PG.QualifiedObject {qSchema = "public", qName = "artists"},
      artistsIdColumn = PG.unsafePGCol id_,
      albumsTableName =
        PG.QualifiedObject {qSchema = "public", qName = "albums"},
      albumsArtistIdColumn = PG.unsafePGCol artist_id
    }

-- Table and column names as variables to prevent typos. This is an experiment.
-- Feel free to revert it.

-- Adding an underscore to avoid name clash
id_ :: Text
id_ = "id"

artists :: Text
artists = "artists"

id_null :: Text
id_null = "id_null"

name :: Text
name = "name"

fav_album :: Text
fav_album = "fav_album"

albums :: Text
albums = "albums"

title :: Text
title = "title"

artist_id :: Text
artist_id = "artist_id"

artist_id_null :: Text
artist_id_null = "artist_id_null"

artist_name :: Text
artist_name = "artist_name"

-- Backend-specific SQL statements

setupSql_postgres :: Text
setupSql_postgres =
  [st|
CREATE TABLE #{artists}
  ( #{id_} SERIAL PRIMARY KEY
  , #{id_null} INTEGER -- for testing null join columns
  , #{name} TEXT NOT NULL
  , #{fav_album} TEXT -- for testing joins over multiple fields
  );

INSERT INTO #{artists} (#{id_null}, #{name}, #{fav_album}) VALUES
  (1,    'Author', 'Album 1A'),
  (null, 'Author', 'Album 2A');

CREATE TABLE #{albums}
  ( #{id_} SERIAL PRIMARY KEY
  , #{title} TEXT NOT NULL
  , #{artist_id} INTEGER REFERENCES artists(#{id_})
  , #{artist_id_null} INTEGER -- for testing null join columns
  , #{artist_name} TEXT -- for testing multiple results in object relationships
  );

INSERT INTO
  #{albums} (#{title}, #{artist_id}, #{artist_id_null}, #{artist_name}) VALUES
  ('Album 1A', 1, 1,    'Author'),
  ('Album 1B', 1, null, 'Author'),
  ('Album 2A', 2, 2,    'Author'),
  ('Album 2B', 2, null, 'Author');
|]
