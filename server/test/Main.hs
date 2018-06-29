{-# LANGUAGE OverloadedStrings #-}

module Main where
import           Data.Bool

import           Control.Monad.Trans.Except

import           System.Exit                (exitFailure)

import           Control.Monad

import           Data.List                         as L

import           Data.Aeson                        as A hiding (Options)

import           Data.Graph.Inductive.Basic        as DB
import           Data.Graph.Inductive.Graph        as G hiding (mkGraph)
import           Data.Graph.Inductive.PatriciaTree as GT
import           Data.Graph.Inductive.Query.DFS    as DFS

import           Data.HashMap.Strict               as HM

import           Network.Wai.Test                  (SResponse(..))

import           Test.Hspec
import           Test.Hspec.Core.Runner
import           Test.Hspec.Core.Spec              hiding (runIO)
import           Test.Hspec.Wai

import qualified Data.Text                         as T

import qualified Data.ByteString.Char8             as BS
import qualified Data.ByteString.Builder           as BB
import qualified Data.ByteString.Lazy.Char8        as BL

import           System.Directory
import           System.Environment                (withArgs)
import           System.FilePath.Posix

import           Hasura.Server.Init
import           Hasura.Prelude
import           Hasura.Server.App                  (ravenLogGen, RavenLogger, app, AuthMode(..))
import           Hasura.Server.Logging              (withStdoutLogger)
import qualified Database.PG.Query                    as Q


import qualified Database.PG.Query                    as PGQ

import           Web.Spock.Core                    (spockT, spockAsApp)

import qualified Network.HTTP.Types                as H
import           Network.Wai                       (Application)

import           Options.Applicative               hiding (action)

defTxMode :: Q.TxMode
defTxMode = (Q.Serializable, Nothing)

data TestItem
     = TestItem
       {
         itRequest    :: !Object,
         itResponse   :: !(Maybe Value),
         itStatusCode :: !Int,
         itRole       :: !String,
         itUserID     :: !String,
         itUrl        :: !String,
         itMethod     :: !String,
         itName       :: !String,
         itSQLCheck   :: !(Maybe [String]),
         itPreSQL     :: !(Maybe [String]),
         itPostSQL   :: !(Maybe [String])
       } deriving (Show, Eq)

instance FromJSON TestItem where
   parseJSON (Object v) =
     TestItem <$> v .: "request"
     <*> v .:? "response"
     <*> v .: "status_code"
     <*> v .: "role"
     <*> v .: "user_id"
     <*> v .: "url"
     <*> v .: "method"
     <*> v .: "name"
     <*> v .:? "sql_check"
     <*> v .:? "pre_sql"
     <*> v .:? "post_sql"

   parseJSON _ = fail "Expected an object for select"

data Dependency = Dependency !FilePath !(Maybe [String])
                  deriving (Show, Eq)

instance FromJSON Dependency where
  parseJSON (String s) =
    return $ Dependency (T.unpack s) Nothing
  parseJSON (Object o) =
    Dependency <$> o .: "filename"
    <*> o .:? "names"
  parseJSON _ = fail "Expected an object for select"

data TestCase
     = TestCase
       {
         itDescription :: !String,
         tcItems       :: ![TestItem],
         tcDepends     :: ![Dependency]
       } deriving (Show, Eq)

instance FromJSON TestCase where
  parseJSON (Object v) =
     TestCase <$> v .: "description"
     <*> v .: "items"
     <*> v .: "depends"
  parseJSON _ = fail "Expected an object for select"


type GraphInfo = (HashMap FilePath Int, GT.Gr FilePath ())

depToFp :: Dependency -> FilePath
depToFp (Dependency x _) = x

fileToTestCase :: FilePath -> IO TestCase
fileToTestCase fp = do
  contents <- BL.readFile fp
  either fail return $ A.eitherDecode contents

consGraph :: GraphInfo -> FilePath -> IO GraphInfo
consGraph (nodeMap, graph) fp = do
  tc <- fileToTestCase fp

  let node = (case HM.lookup fp nodeMap of
               Nothing -> 1 + HM.size nodeMap
               Just x -> x)

      depends = L.map (combine (takeDirectory fp) . depToFp) $ tcDepends tc

  (nodeMap', graph') <- foldM consGraph (HM.insert fp node nodeMap, graph) depends

  let outLinks = L.map (nodeMap' !) depends
      ctxt = ([], node, fp, zip [(),()..] outLinks)

  return $ (nodeMap', ctxt & graph')


mkGraph :: [FilePath] -> IO GraphInfo
mkGraph = foldM consGraph (HM.empty, G.empty)

-- | 'topsort', returning only the labels of the nodes.
topsortFrom :: (Graph gr) => Node -> gr a b -> [a]
topsortFrom node = reverse . postorderF . dffWith go [node] where
  go (_, _, label, _) = label

hasura_req :: TestItem -> WaiSession SResponse
hasura_req ti = request method url [
  ("x-hasura-role", role),
  ("x-hasura-user-id", userID)] body
  where
    method = BS.pack $ itMethod ti
    role = BS.pack $ itRole ti
    userID = BS.pack $ itUserID ti
    url = BS.pack $ itUrl ti
    body = A.encode $ itRequest ti

runSQL :: Q.PGPool -> String -> IO ()
runSQL pool queryStmt = do
  let q = Q.withQE PGQ.PGExecErrTx (Q.fromBuilder $ BB.stringUtf8 queryStmt) () False
  _ <- runExceptT $ Q.runTx pool defTxMode q :: (IO (Either PGQ.PGExecErr Q.Discard))
  return ()

checkSQL :: Q.PGPool -> String -> IO Bool
checkSQL pool queryStmt = do
  let q = Q.withQE PGQ.PGExecErrTx (Q.fromBuilder $ BB.stringUtf8 queryStmt) () False
  res <- liftIO $ runExceptT $ Q.runTx pool defTxMode q
  case res of
    Left x -> print x >> return False
    Right (Q.WithCount n (Q.Discard _)) -> return $ n == 1

matchTestItem :: WaiSession SResponse -> TestItem -> Q.PGPool -> WaiExpectation
matchTestItem action ti pool = do
  liftIO $ case (itPreSQL ti) of
    Nothing -> return ()
    Just sqlExps -> do
      forM_ sqlExps (runSQL pool)

  r <- action

  liftIO $ (H.statusCode $ simpleStatus r) `shouldBe` itStatusCode ti

  case itResponse ti of
    Nothing -> return ()
    Just resp -> do
      jsonBody <- (either (const error resp) return $ A.eitherDecode $ simpleBody r) :: WaiSession A.Value
      let
        this = BL.unpack $ A.encode jsonBody
        that = BL.unpack $ A.encode resp

      if jsonBody == resp then return ()
        else liftIO . expectationFailure $ "For " ++ (itName ti) ++ "\nGot\n" ++ this ++ "\ninstead of\n" ++ that where

  liftIO $ case (itPostSQL ti) of
    Nothing -> return ()
    Just sqlExps -> do
      forM_ sqlExps (runSQL pool)

  liftIO $ case (itSQLCheck ti) of
    Nothing -> return ()
    Just sqlExps -> do
      ress <- forM sqlExps (checkSQL pool)
      mapM_ (\(i, res) -> bool
                          (expectationFailure ("SQL check " ++ show i ++ " failed"))
                          (return ()) res) $ zip [(1 :: Int)..] ress

mkSpecLeaf :: Q.PGPool -> String -> [TestItem] -> [TestItem] -> SpecTree (Arg WaiExpectation)
mkSpecLeaf pool tcDesc tis depTis= specItem tcDesc $ do
  mapM_ hasura_req depTis

  foldM (\_ ti -> matchTestItem (hasura_req ti) ti pool) () tis

mkSpecList :: Q.PGPool -> TestCase -> [TestCase] -> SpecTree (Arg WaiExpectation)
mkSpecList pool (TestCase desc items _) deps = do
  let depItems = L.concatMap tcItems deps
  mkSpecLeaf pool desc items depItems

resetStateTx :: Q.TxE PGQ.PGExecErr ()
resetStateTx = do
  Q.unitQE PGQ.PGExecErrTx "DROP SCHEMA hdb_catalog CASCADE" () False
  Q.unitQE PGQ.PGExecErrTx "DROP SCHEMA raven CASCADE" () False
  Q.unitQE PGQ.PGExecErrTx "DROP SCHEMA raven_views CASCADE" () False
  Q.unitQE PGQ.PGExecErrTx "DROP SCHEMA public CASCADE" () False
  Q.unitQE PGQ.PGExecErrTx "CREATE SCHEMA public" () False

raven_app :: RavenLogger -> PGQ.PGPool -> IO Application
raven_app rlogger pool =
  do
    _ <- liftIO $ runExceptT $ Q.runTx pool defTxMode resetStateTx
    let corsCfg = CorsConfig "*" True  -- cors is disabled
    spockAsApp $ spockT id $ app Q.Serializable Nothing rlogger pool AMNoAuth corsCfg True -- no access key and no webhook

main :: IO ()
main = withStdoutLogger ravenLogGen $ \rlogger -> do
  Options rci cp args <- parseArgs

  ci <- either ((>> exitFailure) . (putStrLn . connInfoErrModifier))
    return $ mkConnInfo rci

  pool <- Q.initPGPool ci cp

  files <- forM args makeAbsolute

  (nodeMap, graph) <- mkGraph files

  specs <- forM files $ \fp -> do
    let node = nodeMap ! fp
        depFiles = L.reverse $ tail $ topsortFrom node graph

    tc <- fileToTestCase fp
    tcs <- forM depFiles fileToTestCase

    return $ mkSpecList pool tc tcs

  withArgs [] $ hspecWith defaultConfig  $ with (raven_app rlogger pool) $ fromSpecList specs


data Options = Options RawConnInfo Q.ConnParams [FilePath]

parseArgs :: IO Options
parseArgs = execParser opts
  where
    optParser = Options <$> parseRawConnInfo <*> parseConnParams <*> some (
      argument str (metavar "FILES..."))

    opts = info (helper <*> optParser)
           ( fullDesc <>
             header "raven-test")
