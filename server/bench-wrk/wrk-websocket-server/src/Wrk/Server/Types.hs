{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Wrk.Server.Types
where

import           Control.Lens               ((^@..))
import           Data.Default               (Default (..))

import qualified Data.Aeson                 as J
import qualified Data.Aeson.Casing          as J
import qualified Data.Aeson.Lens            as J
import qualified Data.Aeson.TH              as J
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T

newtype ErrorMessage = ErrorMessage { getError :: J.Value }
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

newtype Duration = Duration { getDuration :: Int}
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

newtype Threads = Threads { getThreads :: Int}
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

newtype Connections = Connections { getConnections :: Int}
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

newtype Rate = Rate { getRate :: Int}
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

newtype Query = Query { getQuery :: T.Text }
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

type GraphQLURL = String

data ServerConf
  = ServerConf
  { scGraphQLUrl :: !GraphQLURL
  }

data WrkBenchArgs = WrkBenchArgs
  { wbaDuration    :: !(Maybe Duration)
  , wbaThreads     :: !(Maybe Threads)
  , wbaConnections :: !(Maybe Connections)
  , wbaQuery       :: !Query
  , wbaGraphqlUrl  :: !GraphQLURL
  }
  deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 3 J.snakeCase){J.omitNothingFields = True} ''WrkBenchArgs)

data Wrk2BenchArgs = Wrk2BenchArgs
  { w2baDuration    :: !(Maybe Duration)
  , w2baThreads     :: !(Maybe Threads)
  , w2baConnections :: !(Maybe Connections)
  , w2baQuery       :: !Query
  , w2baRate        :: !Rate
  , w2baGraphqlUrl  :: !GraphQLURL
  }
  deriving  (Show, Eq)

$(J.deriveJSON (J.aesonDrop 4 J.snakeCase){J.omitNothingFields = True} ''Wrk2BenchArgs)

-- If the JSON output is a dictionary of pairs (k,v), convert it into set of arguments of the form --k v
toArgsList :: (J.ToJSON a) => (T.Text -> Bool) -> a -> [String]
toArgsList keysFilter a = concatMap toArg $ filter (keysFilter . fst) $ J.toJSON a ^@.. J.members
  where
    toArg (k,v) = ["--" <> T.unpack k, BLC.unpack (J.encode v)]

data BenchConf
  = BCWrk WrkBenchArgs
  | BCWrk2 Wrk2BenchArgs
  deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 2 J.camelCase)
  { J.sumEncoding = J.TaggedObject "framework" "arguments"
  ,  J.constructorTagModifier= J.snakeCase . drop 2
  }
  ''BenchConf)

data HistogramSummary = HistogramSummary
  { hsMean         :: !Double
  , hsStdDeviation :: !Double
  , hsMax          :: !Double
  , hsTotalCount   :: !Integer
  , hsBuckets      :: !Integer
  , hsSubBuckets   :: !Integer
  }
  deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 2 J.camelCase) ''HistogramSummary)

data HistogramRow = HistogramRow
  { hrValue      :: !Double
  , hrPercentile :: !Double
  , hrCount      :: !Integer
  }
  deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 2 J.camelCase) ''HistogramRow)

data HdrHistogram = HdrHistogram
  { hdrSummary :: !HistogramSummary
  , hdrData    :: ![HistogramRow]
  }
  deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 2 J.camelCase) ''HdrHistogram)

type ValuesDist = HM.HashMap Double Double

data StatsSummary = StatsSummary
  { rqsMin   :: Double
  , rqsMax   :: Double
  , rqsMean  :: Double
  , rqsStdev :: Double
  , rqsDist  :: ValuesDist
  }
  deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 3 J.camelCase) ''StatsSummary)

newtype RequestsSummary = RequestsSummary
  { getReqSummary :: StatsSummary }
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

newtype LatencySummary = LatencySummary
  { getLatencySummary :: StatsSummary }
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

data WrkResult = WrkResult
  { wrkSummary  :: J.Value
  , wrkRequests :: RequestsSummary
  }
  deriving (Show, Eq)

type WrkResultOut = WrkResult

$(J.deriveJSON (J.aesonDrop 3 J.camelCase) ''WrkResult)

data Wrk2ResultIn = Wrk2ResultIn
  { wrk2Summary  :: !J.Value
  , wrk2Requests :: !RequestsSummary
  , wrk2Latency  :: !LatencySummary
  }
  deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 4 J.camelCase) ''Wrk2ResultIn)

type LatencyValues = Double

data LatencyResultOut = LatencyResultOut
  { lroRaw       :: ![LatencyValues]
  , lroHistogram :: !HdrHistogram
  , lroSummary   :: !LatencySummary
  }
  deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 3 J.camelCase) ''LatencyResultOut)

data Wrk2ResultOut = Wrk2ResultOut
  { wrk2oSummary  :: !J.Value
  , wrk2oRequests :: !RequestsSummary
  , wrk2oLatency  :: !LatencyResultOut
  }
  deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 5 J.camelCase) ''Wrk2ResultOut)

data BenchResult
  = BRWrk WrkResultOut
  | BRWrk2 Wrk2ResultOut
  deriving (Show, Eq)

$(J.deriveJSON  J.defaultOptions {J.sumEncoding = J.UntaggedValue}
  ''BenchResult)

data BenchMessage
  = BMRunBenchmark
      { bmConfiguration :: !BenchConf }
  | BMStart
      { bmConfiguration :: !BenchConf }
  | BMResult
      { bmConfiguration :: !BenchConf
      , bmResult        :: !BenchResult
      }
  | BMFinish
      { bmConfiguration :: !BenchConf }
  | BMError
      { bmMessage :: ErrorMessage }
  deriving (Show, Eq)

$(J.deriveJSON J.defaultOptions
  { J.sumEncoding = J.ObjectWithSingleField
  , J.constructorTagModifier = J.snakeCase . drop 2
  , J.fieldLabelModifier = J.snakeCase.  drop 2
  }
  ''BenchMessage
 )

newtype WrkScriptsDir = WrkScriptsDir { getWrkScriptsDir :: FilePath}
  deriving (Eq)

instance Show WrkScriptsDir where
  show (WrkScriptsDir d) = d

instance Default WrkScriptsDir where
  def = WrkScriptsDir "../bench_scripts"

newtype WrkScript = WrkScript { getWrkScript :: FilePath }
  deriving (Eq)

instance Show WrkScript where
  show (WrkScript s) = s

instance Default WrkScript where
  def = WrkScript $ show (def :: WrkScriptsDir) <> "/bench-wrk.lua"

newtype Wrk2Script = Wrk2Script { getWrk2Script :: FilePath }
  deriving (Eq)

instance Show Wrk2Script where
  show (Wrk2Script s) = s

instance Default Wrk2Script where
  def = Wrk2Script $ show (def :: WrkScriptsDir) <> "/bench-wrk2.lua"
