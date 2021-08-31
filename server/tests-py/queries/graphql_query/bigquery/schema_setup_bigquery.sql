DROP SCHEMA IF EXISTS `<<<PROJECT_ID>>>.hasura` CASCADE;
CREATE SCHEMA `<<<PROJECT_ID>>>.hasura`;
CREATE TABLE `hasura.all_types` (
  `string` STRING,
  `bytes` BYTES,
  `integer` INT64,
  `float` FLOAT64,
  `numeric` NUMERIC,
  `bignumeric` BIGNUMERIC,
  `boolean` BOOL,
  `timestamp` TIMESTAMP,
  `date` DATE,
  `time` TIME,
  `datetime` DATETIME,
  `geography` GEOGRAPHY
);
CREATE TABLE `hasura.author` (
  `id` INT64,
  `name` STRING,
  `created_at` TIMESTAMP
);
CREATE TABLE `hasura.article` (
  `id` INT64,
  `title` STRING,
  `content` STRING,
  `author_id` INT64,
  `is_published` BOOL,
  `published_on` DATETIME,
  `created_at` TIMESTAMP
);
INSERT INTO `hasura.all_types` VALUES (
  "STRING",
  CODE_POINTS_TO_BYTES([0,1,2,3,4,5]),
  1,
  1,
  1,
  1,
  true,
  PARSE_TIMESTAMP("%c", "Thu Dec 25 07:30:00 2008"),
  PARSE_DATE("%F", "2000-12-30"),
  PARSE_TIME("%T", "07:30:00"),
  PARSE_DATETIME('%Y-%m-%d %H:%M:%S', '1998-10-18 13:45:55'),
  ST_GEOGPOINT(1, 1)
);
INSERT INTO `hasura.author` VALUES (
  1, "Author 1", PARSE_TIMESTAMP("%c", "Thu Dec 25 07:30:00 2008")
);
INSERT INTO `hasura.author` VALUES (
  2, "Author 2", PARSE_TIMESTAMP("%c", "Thu Dec 25 07:30:00 2008")
);
INSERT INTO `hasura.article` VALUES (
  1, "Title 1", "Content 1", 1, FALSE, NULL, PARSE_TIMESTAMP("%c", "Thu Dec 25 07:30:01 2008")
);
INSERT INTO `hasura.article` VALUES (
  2, "Title 2", "Content 2", 1, TRUE, PARSE_DATETIME('%Y-%m-%d %H:%M:%S', '1998-10-18 13:45:55'), PARSE_TIMESTAMP("%c", "Thu Dec 25 07:30:02 2008")
);
INSERT INTO `hasura.article` VALUES (
  3, "Title 3", "Content 3", 2, FALSE, NULL, PARSE_TIMESTAMP("%c", "Thu Dec 25 07:30:03 2008")
);

-- These are helpful for join/aggregate testing.

CREATE TABLE hasura.Artist (
 artist_self_id int64,
 name string);

CREATE TABLE hasura.Album (
  album_self_id int64,
  artist_other_id int64,
  title string);

INSERT INTO hasura.Artist
VALUES      (1002,
             "tool" ),
            (1000,
             "alice in chains" ),
            (1001,
             "nirvana" );

INSERT INTO hasura.Album
            (album_self_id,
             title,
             artist_other_id)
VALUES      ( 2002,
              "dirt",
              1000 ),
            ( 2005,
              "facelift",
              1000 ),
            ( 2001,
              "in utero",
              1001 ),
            ( 2000,
              "nevermind",
              1001 ),
            ( 2003,
              "lateralus",
              1002 ),
            ( 2004,
              "fear innoculum",
              1002 );

-- These are for testing global limits.

CREATE TABLE hasura.LimitedArtist (
 artist_self_id int64,
 name string);

CREATE TABLE hasura.LimitedAlbum (
  album_self_id int64,
  artist_other_id int64,
  title string);

INSERT INTO hasura.LimitedArtist
VALUES      (1002,
             "tool" ),
            (1000,
             "alice in chains" ),
            (1001,
             "nirvana" );

INSERT INTO hasura.LimitedAlbum
            (album_self_id,
             title,
             artist_other_id)
VALUES      ( 2002,
              "dirt",
              1000 ),
            ( 2005,
              "facelift",
              1000 ),
            ( 2001,
              "in utero",
              1001 ),
            ( 2000,
              "nevermind",
              1001 ),
            ( 2003,
              "lateralus",
              1002 ),
            ( 2004,
              "fear innoculum",
              1002 );
