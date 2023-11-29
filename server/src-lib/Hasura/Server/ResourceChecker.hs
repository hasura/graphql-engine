module Hasura.Server.ResourceChecker
  ( getServerResources,
    ComputeResourcesResponse (..),
    ResourceCheckerError (..),

    -- * Exposed for testing
    getMaxPhysicalMemory,
    getPhysicalCpuResource,
    getPhysicalResources,
    getServerResources_,
    getCGroupV1Resources,
    getCGroupV2Resources,
  )
where

import Data.Aeson (ToJSON (toJSON))
import Data.Aeson qualified as J
import Data.Bifunctor (Bifunctor (bimap))
import Data.Int (Int64)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T
import GHC.Conc (getNumProcessors)
import Hasura.Prelude
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.IO.Error (catchIOError)

-- | The response data of cpu and memory resources
data ComputeResourcesResponse = ComputeResourcesResponse
  { _rcrCpu :: Maybe Int,
    _rcrMemory :: Maybe Int64,
    _rcrErrorCode :: Maybe ResourceCheckerError
  }
  deriving (Generic, Eq, Show)

instance J.ToJSON ComputeResourcesResponse where
  toJSON :: ComputeResourcesResponse -> J.Value
  toJSON = J.genericToJSON hasuraJSON {J.omitNothingFields = True}

data CGroupMode
  = CGUnavailable
  | CGroupV1
  | CGroupV2
  deriving (Eq, Show)

data ResourceCheckerError
  = CGroupUnavailable
  | CpuInconclusive
  | MemoryInconclusive
  | CpuMemoryInconclusive
  | RCInternalError String
  deriving (Eq)

instance Show ResourceCheckerError where
  show = \case
    CGroupUnavailable -> "CGROUP_UNAVAILABLE"
    CpuInconclusive -> "CPU_INCONCLUSIVE"
    MemoryInconclusive -> "MEMORY_INCONCLUSIVE"
    CpuMemoryInconclusive -> "CPU_MEMORY_INCONCLUSIVE"
    RCInternalError err -> err

instance J.ToJSON ResourceCheckerError where
  toJSON = J.toJSON . show

perCpuShares :: Int
perCpuShares = 1024

-- | Limit the maximum memory capacity of the physical server
-- The resource checker will returns the inconclusive memory error if the value exceeds this limit
-- according to Red Hat limits https://access.redhat.com/articles/rhel-limits
maximumMemoryLimitBytes :: Int64
maximumMemoryLimitBytes = 256 * (1024 ^ (4 :: Int64)) -- 256 TiBs

-- | Determine allocated cpu and memory resources of the host server or Container Runtime.
-- because HGE mainly runs in the container runtime
-- we need to determine the max cpu and memory limit constraints
-- that are managed by cgroups or fallback to physical cpu and memory information of the server
-- https://hasurahq.atlassian.net/browse/INFRA-772
--
-- Those information are stored in many files of cgroup folders,
-- the logic is simply to read them and parse number values
--
-- In cgroup v1 systems there are several ways in which the amount of allocated cpu resources could be presented.
-- We first try reading requests (quota & period); if that fails, we fallback to reading limits (shares);
-- if that fails, we fallback to reading the physical cpu count, which should always succeed.

-- Some cgroup v2 systems still use the cgroup v1 file structure (hybrid mode). To account for those systems,
-- we fallback to the above cgroup v1 logic if we fail to read the allocated cpu resources in the expected way.
getServerResources :: (MonadIO m) => m ComputeResourcesResponse
getServerResources = getServerResources_ "/proc/self/mountinfo"

getServerResources_ :: (MonadIO m) => FilePath -> m ComputeResourcesResponse
getServerResources_ mountPath =
  getCGroupMode >>= \case
    (CGUnavailable, _) -> getPhysicalResources (Just CGroupUnavailable)
    (CGroupV1, cgroupRoot) -> getCGroupV1Resources cgroupRoot
    (CGroupV2, cgroupRoot) -> getCGroupV2Resources cgroupRoot
  where
    -- find the line that contains the cgroup folder path in the mount info file
    -- the line should have from 9 words, for example:
    -- 29 21 0:25 / /sys/fs/cgroup rw,nosuid,nodev,noexec,relatime shared:9 - cgroup2 cgroup2
    --
    -- in cgroup v1 there can be many cgroup lines with child paths
    -- we need to find the root cgroup folder
    -- 1269 1263 0:31 /kubepods/burstable/pod37349393 /sys/fs/cgroup/blkio ro,nosuid,nodev,noexec,relatime master:14 - cgroup cgroup rw,blkio
    getCGroupMode =
      liftIO
        $ catchIOError
          ( T.readFile mountPath
              >>= ( \contentLines ->
                      case find (\ls -> ("cgroup" `elem` ls || "cgroup2" `elem` ls) && length ls >= 9) contentLines of
                        Nothing -> return (CGUnavailable, "")
                        Just ls ->
                          let cgroupPath = T.unpack (ls !! 4)
                              cgroupRoot = bool (takeDirectory cgroupPath) cgroupPath ("cgroup2" `elem` ls)
                           in liftIO (doesFileExist (cgroupRoot </> "cgroup.controllers"))
                                <&> bool CGroupV1 CGroupV2
                                <&> (,cgroupRoot)
                  )
              . map T.words
              . T.lines
          )
          (return . const (CGUnavailable, ""))

-- | Compute the cpu share allocations from the number of physical CPU cores
getPhysicalCpuResource :: (MonadIO m) => m Int
getPhysicalCpuResource = liftIO getNumProcessors <&> (* perCpuShares)

-- | Compute the max physical memory size of the server
getMaxPhysicalMemory :: (MonadIO m) => m (Maybe Int64)
getMaxPhysicalMemory = liftIO $ catchIOError readMemory (return . const Nothing)
  where
    -- the value of meminfo is in KB
    parseMemoryBytes l =
      if length (T.words l) < 2
        then Nothing
        else either (const Nothing) (Just . (* 1024)) (parseUint @Int64 (T.words l !! 1))

    readMemory =
      T.readFile "/proc/meminfo" >>= \c ->
        let l = find ("MemTotal:" `T.isPrefixOf`) (map T.strip (T.lines c))
         in return $ parseMemoryBytes =<< l

getPhysicalResources :: (MonadIO m) => Maybe ResourceCheckerError -> m ComputeResourcesResponse
getPhysicalResources err = do
  cpu <- getPhysicalCpuResource
  maxMem <- getMaxPhysicalMemory
  return $ ComputeResourcesResponse (Just cpu) maxMem err

-- | Determine cpu and memory resource allocations
-- if the OCI Container Runtime supports cgroup v1
getCGroupV1Resources :: (MonadIO m) => FilePath -> m ComputeResourcesResponse
getCGroupV1Resources cgroupRoot = do
  (cpu, cpuErr) <- catchCpuAllocation $ getCGroupV1CpuAllocation cgroupRoot
  (memMax, memErr) <- catchMemoryAllocation $ getCGroupV1MemoryAllocation cgroupRoot
  return $ ComputeResourcesResponse (Just cpu) memMax (mergeCpuMemoryErrors cpuErr memErr)

getCGroupV1CpuAllocation ::
  (MonadIO m, MonadError ResourceCheckerError m) =>
  FilePath ->
  m (Int, Maybe ResourceCheckerError)
getCGroupV1CpuAllocation cgroupRoot = cpuLimits `catchError` const cpuShares
  where
    readCpuValue name = readFileUint (const CpuInconclusive) (cgroupRoot </> "cpu" </> name)

    cpuShares = do
      shares <- readCpuValue "cpu.shares"
      when (shares <= 2 || shares == 1024) $ throwError (RCInternalError "INVALID_CPU_SHARES")
      return (shares, Nothing)

    cpuLimits = do
      cq <- readCpuValue "cpu.cfs_quota_us"
      cp <- readCpuValue "cpu.cfs_period_us"
      liftEither $ deduceCpuLimits cq cp

getCGroupV1MemoryAllocation ::
  (MonadIO m, MonadError ResourceCheckerError m) =>
  FilePath ->
  m (Maybe Int64, Maybe ResourceCheckerError)
getCGroupV1MemoryAllocation cgroupRoot =
  getMemoryAllocation (cgroupRoot </> "memory" </> "memory.limit_in_bytes")

-- | Determine cpu and memory resource allocations
-- if the OCI Container Runtime supports cgroup v2
getCGroupV2Resources :: (MonadIO m) => FilePath -> m ComputeResourcesResponse
getCGroupV2Resources cgroupRoot = do
  (cpu, cpuErr) <- getCpuAllocationCGroupV2
  (memMax, memErr) <- getMemoryAllocationCGroupV2
  return $ ComputeResourcesResponse (Just cpu) memMax (mergeCpuMemoryErrors cpuErr memErr)
  where
    -- CPU shares (OCI) value needs to get translated into
    -- a proper CGroups v2 value. See:
    -- https://github.com/containers/crun/blob/master/crun.1.md#cpu-controller
    --
    -- Use the inverse of (x == OCI value, y == cgroupsv2 value):
    -- ((262142 * y - 1)/9999) + 2 = x
    cpuShares = do
      shares' <- readFileUint (const CpuInconclusive) (cgroupRoot </> "cpu.weight")
      when (shares' <= 2 || shares' == 100) $ throwError (RCInternalError "INVALID_CPU_SHARES")
      return ((262142 * (shares' - 1) `div` 9999) + 2, Nothing)

    parsePeriod = mapLeft (const $ RCInternalError "INVALID_CPU_PERIOD") . parseUint
    parseQuota = mapLeft (const $ RCInternalError "INVALID_CPU_QUOTA") . parseUint

    cpuLimits = do
      content <- readFileT (const CpuInconclusive) (cgroupRoot </> "cpu.max")
      liftEither $ case map T.strip (T.words content) of
        [quota, period] -> uncurry deduceCpuLimits =<< (,) <$> parseQuota quota <*> parsePeriod period
        _ -> Left $ RCInternalError "INVALID_CPU_PERIOD_AND_QUOTA"

    getCpuAllocationCGroupV2 =
      catchCpuAllocation
        $ cpuLimits
        `catchError` const cpuShares
        `catchError` const (getCGroupV1CpuAllocation cgroupRoot)

    getMemoryAllocationCGroupV2 =
      catchMemoryAllocation
        $ getMemoryAllocation (cgroupRoot </> "memory.max")
        `catchError` const (getCGroupV1MemoryAllocation cgroupRoot)

deduceCpuLimits :: Int -> Int -> Either ResourceCheckerError (Int, Maybe ResourceCheckerError)
deduceCpuLimits quota period
  | quota <= 0 = Left $ RCInternalError "INVALID_CPU_QUOTA"
  | period <= 0 = Left $ RCInternalError "INVALID_CPU_PERIOD"
  | otherwise = Right (ceiling (toRational quota / toRational period * toRational perCpuShares), Nothing)

getMemoryAllocation ::
  (MonadIO m, MonadError ResourceCheckerError m) =>
  FilePath ->
  m (Maybe Int64, Maybe ResourceCheckerError)
getMemoryAllocation path = do
  content <- readFileT (const MemoryInconclusive) path <&> T.strip
  mMaxPhysicalMemory <- getMaxPhysicalMemory
  return
    $ if content == "max"
      then (mMaxPhysicalMemory, Nothing)
      else case parseUint content of
        Left e -> (mMaxPhysicalMemory, Just e)
        -- the cgroup memory limit config should be smaller or equal the max physical memory
        Right mem ->
          if mem <= fromMaybe maximumMemoryLimitBytes mMaxPhysicalMemory
            then (Just mem, Nothing)
            else (mMaxPhysicalMemory, Just MemoryInconclusive)

-- catch cpu allocation error with default physical cpu resource
catchCpuAllocation ::
  (MonadIO m) =>
  ExceptT e m (Int, Maybe ResourceCheckerError) ->
  m (Int, Maybe ResourceCheckerError)
catchCpuAllocation m =
  runExceptT m
    >>= flip onLeft (\_ -> getPhysicalCpuResource <&> (,Just CpuInconclusive))

-- catch memory allocation error with default physical memory resource
catchMemoryAllocation ::
  (MonadIO m) =>
  ExceptT ResourceCheckerError m (Maybe Int64, Maybe ResourceCheckerError) ->
  m (Maybe Int64, Maybe ResourceCheckerError)
catchMemoryAllocation m =
  runExceptT m
    >>= flip onLeft (\e -> getMaxPhysicalMemory <&> (,Just e))

mergeCpuMemoryErrors :: Maybe ResourceCheckerError -> Maybe ResourceCheckerError -> Maybe ResourceCheckerError
mergeCpuMemoryErrors cpuErr memErr = case (cpuErr, memErr) of
  (Nothing, Nothing) -> Nothing
  (Just e1, Nothing) -> Just e1
  (Nothing, Just e2) -> Just e2
  (Just CpuInconclusive, Just MemoryInconclusive) -> Just CpuMemoryInconclusive
  (Just e1, Just e2) -> Just $ RCInternalError (show e1 <> "|" <> show e2)

readFileT :: (MonadIO m, MonadError ResourceCheckerError m) => (String -> ResourceCheckerError) -> FilePath -> m T.Text
readFileT mapError path = do
  eContent <- liftIO $ catchIOError (Right <$> T.readFile path) (pure . Left . show)
  liftEither $ mapLeft mapError eContent

parseUint :: (Integral a) => T.Text -> Either ResourceCheckerError a
parseUint = bimap RCInternalError fst . T.decimal

readFileUint ::
  (MonadIO m, MonadError ResourceCheckerError m, Integral a) =>
  (String -> ResourceCheckerError) ->
  FilePath ->
  m a
readFileUint mapError p = (liftEither . parseUint) =<< readFileT mapError p
