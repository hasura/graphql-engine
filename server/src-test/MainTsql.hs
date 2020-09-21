-- | Tests for Tsql conversion.

module Main where

import           Data.Functor.Identity
import           Data.Proxy
import           Data.String
import qualified Database.ODBC.SQLServer as Odbc
import qualified Hasura.SQL.Tsql.Translate as Translate
import qualified Hasura.SQL.Tsql.Types as Tsql
import           Prelude
import           System.Environment
import           Test.Hspec

main :: IO ()
main = hspec spec

connect :: IO Odbc.Connection
connect = do
  connectionString <- getEnv "CONNSTR"
  Odbc.connect (fromString connectionString)

spec :: SpecWith ()
spec = do
  describe "Compile check" pureTests
  describe
    "Connected tests"
    (beforeAll connect (afterAll Odbc.close connectedTests))

pureTests :: Spec
pureTests = do
  it
    "Select"
    (shouldBe
       (runIdentity (Translate.runTranslate (Translate.fromSelect Proxy)))
       Tsql.Select)
  it
    "Expression"
    (shouldBe
       (runIdentity (Translate.runTranslate (Translate.fromExpression Proxy)))
       Tsql.Expression)

connectedTests :: SpecWith Odbc.Connection
connectedTests = sanity

sanity :: SpecWith Odbc.Connection
sanity =
  it
    "Query sanity check"
    (\connection ->
       shouldReturn (Odbc.query connection "select 1") [Identity (1 :: Int)])
