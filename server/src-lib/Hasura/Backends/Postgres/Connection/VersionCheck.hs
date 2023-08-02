-- | Check version compatability against postgres-like flavors.
module Hasura.Backends.Postgres.Connection.VersionCheck
  ( runCockroachVersionCheck,
    CockroachDbVersion (..),
    parseCrdbVersion,
    crdbVersionIsSupported,
  )
where

import Data.Aeson (object, (.=))
import Data.Aeson.Types (Pair)
import Data.Environment qualified as Env
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection qualified as PG
import Hasura.Backends.Postgres.Connection.Connect (withPostgresDB)
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Common (SourceName)
import Text.Parsec qualified as P
import Text.Parsec.Text qualified as P

-- * Cockroach

-- | Cockroach version
data CockroachDbVersion = CockroachDbVersion
  { crdbMajor :: Word,
    crdbMinor :: Word,
    crdbPatch :: Word,
    -- | includes additional information such as "-beta.4"
    crdbRest :: String
  }
  deriving (Eq, Show)

-- | Check cockroachdb version compatability.
runCockroachVersionCheck :: Env.Environment -> SourceName -> PG.PostgresConnConfiguration -> IO (Either QErr ())
runCockroachVersionCheck env sourceName connConf = do
  result <-
    withPostgresDB env sourceName connConf
      $ PG.rawQE PG.dmlTxErrorHandler (PG.fromText "select version();") [] False
  pure case result of
    -- running the query failed
    Left err ->
      Left err
    -- running the query succeeded
    Right (PG.SingleRow (Identity versionString)) ->
      case parseCrdbVersion versionString of
        -- parsing the query output failed
        Left err ->
          Left
            $ crdbVersionCheckErr500
              [ "version-parse-error" .= show err,
                "version-string" .= versionString
              ]
        -- parsing the query output succeeded
        Right crdbVersion ->
          if crdbVersionIsSupported crdbVersion
            then -- the crdb version is supported
              Right ()
            else -- the crdb version is not supported

              Left
                $ crdbVersionCheckErr500
                  [ "version-string" .= versionString
                  ]

crdbVersionCheckErr500 :: [Pair] -> QErr
crdbVersionCheckErr500 extra =
  ( err500
      ValidationFailed
      "Unsupported CockroachDB version. Supported versions: v22.2 onwards."
  )
    { qeInternal = Just $ ExtraInternal (object extra)
    }

-- | Check version is >= 22.2.0
-- https://hasura.io/docs/latest/databases/postgres/cockroachdb/index
crdbVersionIsSupported :: CockroachDbVersion -> Bool
crdbVersionIsSupported CockroachDbVersion {crdbMajor, crdbMinor, crdbPatch} =
  (crdbMajor > 22)
    || (crdbMajor == 22 && crdbMinor > 2)
    || (crdbMajor == 22 && crdbMinor == 2 && crdbPatch >= 0)

-- | Parse a cockroachDB version string
parseCrdbVersion :: Text -> Either P.ParseError CockroachDbVersion
parseCrdbVersion versionString = P.parse crdbVersionParser "select version();" versionString

-- | Cockroach DB version parser.
-- Example version string:
-- > "CockroachDB CCL v22.2.0-beta.4 (x86_64-pc-linux-gnu, built 2022/10/17 14:34:07, go1.19.1)"
crdbVersionParser :: P.Parser CockroachDbVersion
crdbVersionParser = do
  _ <- P.string "CockroachDB"
  _ <- P.space
  _distribution <- word
  _ <- P.space
  _ <- P.char 'v'
  crdbMajor <- read <$> P.many P.digit
  _ <- P.char '.'
  crdbMinor <- read <$> P.many P.digit
  _ <- P.char '.'
  crdbPatch <- read <$> P.many P.digit
  crdbRest <- word
  pure CockroachDbVersion {..}

word :: P.ParsecT Text u Identity String
word = P.many (P.alphaNum <|> P.oneOf "-~!@#$%^&*=_.,")
