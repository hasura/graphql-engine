{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Wrk.Server
where

import           Wrk.Server.Types

import           Control.Applicative          (liftA2, many, (<|>))
import           Control.Concurrent           (forkIO)
import           Control.Lens                 ((^.), (^?))
import           Control.Monad                (forever, unless, void, when)
import           Control.Monad.Except         (MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           System.Environment           (lookupEnv, setEnv)
import           System.FilePath.Posix        (takeDirectory)

import qualified Control.Concurrent.STM       as STM
import qualified Control.Concurrent.STM.TMVar as TMVar
import qualified Control.Exception            as E
import qualified Data.Aeson                   as J
import qualified Data.Aeson.Lens              as J
import qualified Data.Attoparsec.Text         as AT
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.Lazy.Char8   as BLC
import qualified Data.Default                 as Def
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import qualified Data.Text.Read               as T
import qualified Network.WebSockets           as WS
import qualified Network.Wreq                 as NW
import qualified System.Directory             as Dir
import qualified System.Exit                  as SE
import qualified System.Process               as Proc

benchWsApp :: TMVar.TMVar () -> WS.ServerApp
benchWsApp lock pending = do
 conn <- WS.acceptRequest pending
 WS.withPingThread conn 30 (return ()) $
   forever $ do
     msg <- WS.receiveData conn
     fork_ $ processReq conn msg lock
 where
   fork_ = void . forkIO

processReq :: WS.Connection -> BLC.ByteString -> TMVar.TMVar () -> IO ()
processReq conn msg lock = do
  resE <- catchIO $ runExceptT $ processReq' conn msg lock
  either sendErrMsg sendResult resE
  where
    sendJson = WS.sendTextData conn . J.encode
    sendErrMsg = sendJson . BMError
    sendResult = sendJson . uncurry BMResult
    catchIO :: IO (Either ErrorMessage a) -> IO (Either ErrorMessage a)
    catchIO f = do
      resIOE <- E.try f
      either (return . Left . ioExToErr) return resIOE
    ioExToErr :: E.SomeException -> ErrorMessage
    ioExToErr e = ErrorMessage $ J.object ["IOError" J..= show e ]

processReq' :: (MonadIO m, MonadError ErrorMessage m)
  => WS.Connection -> BL.ByteString -> TMVar.TMVar ()
  -> m (BenchConf, BenchResult)
processReq' conn msg lock = do
  conf <- parseIncomingMsg
  res <- withLock $ runBench conn conf
  return (conf, res)
  where
    parseIncomingMsg = eitherToMonadErr (ErrorMessage . J.toJSON) $ J.eitherDecode msg
    withLock f = do
      locked <- acquireLock
      unless locked $ throwError $ ErrorMessage $ J.toJSON ("A benchmark is already running" :: T.Text)
      resE <- liftIO $ flip E.finally releaseLock $ runExceptT f
      eitherToMonadErr id resE
    acquireLock = atomic_ $ TMVar.tryPutTMVar lock ()
    releaseLock = STM.atomically $ TMVar.takeTMVar lock
    atomic_ = liftIO . STM.atomically

runBench :: (MonadIO m, MonadError ErrorMessage m)
  => WS.Connection -> BenchConf -> m BenchResult
runBench conn conf= do
  sendStartMsg
  result <- case conf of
    BCWrk args  -> BRWrk <$> runWrkBench args
    BCWrk2 args -> BRWrk2 <$> runWrk2Bench args
  void sendFinishMsg
  return result
  where
     sendStartMsg = sendJson $ BMStart conf
     sendFinishMsg = sendJson $ BMFinish conf
     sendJson = liftIO . WS.sendTextData conn . J.encode

runWrkBench :: (MonadIO m, MonadError ErrorMessage m)
 => WrkBenchArgs -> m WrkResultOut
runWrkBench args@WrkBenchArgs{..} = do
   _ <- liftIO runQueryOnce -- run the GraphQL query once to ensure there are no errors
   script <- liftIO wrkScript
   liftIO $ setLuaEnv $ show script
   (exitCode, _, stderr) <- liftIO $ Proc.readProcessWithExitCode "wrk" (wrkArgs script) ""
   liftIO $ putStr stderr
   case exitCode of
      SE.ExitSuccess   -> wrkResult stderr
      SE.ExitFailure _ -> throwError $ ErrorMessage $ J.toJSON $ "Failed with error " <> stderr
   where
     runQueryOnce = runQuery wbaGraphqlUrl wbaQuery
     wrkResult stderr = jsonDecode $ BLC.pack stderr
     wrkArgs script = toArgsList (`notElem` ["query","graphql_url"]) args <> luaScriptArgs script
     luaScriptArgs script = ["-s", show script, wbaGraphqlUrl, T.unpack $ getQuery wbaQuery]
     wrkScript = maybe Def.def WrkScript <$> lookupEnv "HASURA_BENCH_WRK_LUA_SCRIPT"

setLuaEnv :: FilePath -> IO ()
setLuaEnv wrkScript = do
  setEnv "LUA_PATH" $ "/usr/share/lua/5.1/?.lua;" <> wrkScriptDir <> "/?.lua"
  setEnv "LUA_CPATH" "/usr/lib/lua/5.1/?.so;/usr/lib/x86_64-linux-gnu/lua/5.1/?.so;;"
  where wrkScriptDir = takeDirectory wrkScript

-- TODO duration cannot be less than 10 seconds
runWrk2Bench :: (MonadIO m, MonadError ErrorMessage m)
  => Wrk2BenchArgs -> m Wrk2ResultOut
runWrk2Bench args@Wrk2BenchArgs{..} = do
  _ <- liftIO runQueryOnce -- run the GraphQL query once to ensure there are no errors
  liftIO $ Dir.createDirectoryIfMissing True resultsDir
  script <- liftIO wrk2Script
  liftIO $ setLuaEnv $ show script
  (exitCode, stdout, stderr) <- liftIO $ Proc.readProcessWithExitCode "wrk2" (wrk2Args script) ""
  --liftIO $ putStr stderr
  liftIO $ putStr stdout
  case exitCode of
    SE.ExitSuccess   -> wrk2Result stdout
    SE.ExitFailure e -> throwError $ ErrorMessage $ J.toJSON $
      "wrk2 exited with ExitCode" <> show e <> "\nError: " <> stderr
   where
     runQueryOnce = runQuery w2baGraphqlUrl w2baQuery
     wrk2Result stdout = do
       resultStr <- liftIO $ BLC.readFile summaryFile
       -- Read summary from summary file
       resultIn <- jsonDecode resultStr
       -- Parse histogram values from stdout
       histogram <- eitherToMonadErr (ErrorMessage . J.toJSON) $ AT.parseOnly histogramParser $ T.pack stdout
       -- Read latency values from latencies-file
       strLatencies <- liftIO $ T.lines <$> T.readFile latenciesFile
       -- Convert to number from string
       numLatencies <- eitherToMonadErr asErrMessage $
         mapM ( fmap (/1000.0) . readDoubleT) strLatencies
       return $ makeResultOut resultIn histogram numLatencies

     makeResultOut (Wrk2ResultIn summ reqSumm latSumm) hist latVals =
       Wrk2ResultOut summ reqSumm $ LatencyResultOut latVals hist latSumm

     wrk2Args script = toArgsList (`notElem` ["query","graphql_url"]) args <> ["--latency"] <> luaScriptArgs script
     luaScriptArgs script = ["-s", show script] <> [ w2baGraphqlUrl, query, resultsDir ]
     query = T.unpack $ getQuery w2baQuery
     resultsDir = "/tmp/results"
     latenciesFile = resultsDir <> "/latencies"
     summaryFile = resultsDir <> "/summary.json"
     wrk2Script = maybe Def.def Wrk2Script <$> lookupEnv "HASURA_BENCH_WRK2_LUA_SCRIPT"

runQuery :: GraphQLURL -> Query -> IO BLC.ByteString
runQuery url query = do
  resp <- NW.post url $ J.object [ "query" J..= getQuery query ]
  let err = foldl1 (<|>) $ map (\x -> resp ^? NW.responseBody . J.key x) ["errors", "error"]
  maybe (return $ resp ^. NW.responseBody) (fail . BLC.unpack . J.encode ) err

simpleIntrospectQuery :: Query
simpleIntrospectQuery = Query "\
   \ query foo { \
   \   __schema { \
   \     queryType { \
   \       kind \
   \     } \
   \   } \
   \ }"


jsonDecode :: (J.FromJSON a, MonadError ErrorMessage m) => BLC.ByteString -> m a
jsonDecode = eitherToMonadErr (ErrorMessage . J.toJSON) . J.eitherDecode

asErrMessage :: String -> ErrorMessage
asErrMessage = ErrorMessage . J.toJSON

eitherToMonadErr :: (MonadError e' m) => (e -> e') -> Either e a -> m a
eitherToMonadErr modifyErr = either (throwError . modifyErr) return

readDoubleT :: T.Text -> Either String Double
readDoubleT s = do
  (a, rest) <- T.double s
  unless (T.null rest) $ Left "Not a rational number"
  return a

histogramParser :: AT.Parser HdrHistogram
histogramParser = do
  -- Histogram starts in the line after this match
  _ <- takeIncludingFirstMatch "Detailed Percentile spectrum" AT.<?> "Histogram start heading"
  -- Take the rest of the line
  void takeLineText AT.<?> "Till the end of histogram start heading"
  -- Take upto the next line with histogram headers.
  _ <- takeIncludingFirstMatch "Value" AT.<?> "Till Histogram headers"
  void takeLineText AT.<?> "Till Histogram headers"
  -- Take the next empty line
  void AT.skipSpace AT.<?> "Skip space till histogram values"
  -- The values start here
  histRows <- parseHistogramRows AT.<?> "Histogram rows"
  histSummary <- parseHistogramSummary
  return $ HdrHistogram histSummary histRows
  where
    parseHistogramRows = many parseHistogramRow

    parseHistogramSummary = do
      (mean, stdDev) <- collectSummaryPairs
        ("Mean", AT.double, "StdDeviation", AT.double)
      (maxVal, tc) <- collectSummaryPairs
        ("Max",  AT.double, "Total count" , AT.decimal)
      (bkts, subBkts) <- collectSummaryPairs
        ("Buckets", AT.decimal, "SubBuckets", AT.decimal)
      return $ HistogramSummary mean stdDev maxVal tc bkts subBkts

    collectSummaryPairs (name1,parser1,name2,parser2) = do
      _ <- AT.string ("#[" <> name1) AT.<?> ("1. Parse  " <> T.unpack name1)
      void spacedEqualTo AT.<?> ("2. Parse  " <> T.unpack name1)
      d1 <- parser1
      _ <- AT.string (", " <> name2) AT.<?> ("1. Parse " <> T.unpack name2)
      void spacedEqualTo AT.<?> ("2. Parse  " <> T.unpack name1)
      d2 <- parser2
      void takeLineText
      return (d1, d2)

    spacedEqualTo = AT.skipSpace >> AT.char '=' >> AT.skipSpace

    parseHistogramRow = do
      char <- AT.peekChar'
      when (char == '#') $ fail "Reached histogram summary"
      val <- withSkipSpace AT.double
      percentile <- withSkipSpace AT.double
      count <- withSkipSpace AT.decimal
      void takeLineText
      return $ HistogramRow val percentile count

    withSkipSpace p = AT.skipSpace >> p

takeLineText :: AT.Parser T.Text
takeLineText = AT.takeTill AT.isEndOfLine <* AT.endOfLine

uptoFirstMatch :: T.Text -> AT.Parser T.Text
uptoFirstMatch str = fmap T.concat $ AT.manyTill nextPossibleMatch $ AT.string str
  where
    nextPossibleMatch = liftA2 T.cons AT.anyChar $ AT.takeWhile (/= T.head str)

takeIncludingFirstMatch :: T.Text -> AT.Parser T.Text
takeIncludingFirstMatch str = withSubStr <|> errMsg
  where
    withSubStr = fmap (`T.append` str) $ uptoFirstMatch str
    errMsg = fail $ "Could not find sub-string: " <> T.unpack str
