-- | The haskell runtime does not consider cgroup limits (such as are used by
-- kubernetes and docker to size containers) when deciding the number of
-- capabilities to run with `+RTS -N`. This module is about trying to set a
-- good default for `-N` automatically at runtime by considering cgroup limits.
--
-- If we don't do this we risk poor performance as the OS deschedules our
-- capability threads, GC is performed less promptly, and we have extra memory
-- usage for the nursery on each unnecessary capability (and users will observe
-- this as  increased memory usage if they bump up the size of the instance
-- hosting their pods, say,  or even slowly growing memory leak -like
-- behavior).
module Hasura.CpuDetect (tryAutoSetNumCapabilities) where

import Control.Exception
import Data.SerializableBlob qualified as SB
import Data.String
import GHC.Conc
import Hasura.Logging
import Hasura.Prelude
import System.Directory (doesDirectoryExist, doesFileExist)

-- | Try to intelligently 'setNumCapabilities' taking into account cgroups CPU
-- limits. This should be called just once, near the top of each server Main.
tryAutoSetNumCapabilities ::
  Logger Hasura ->
  IO ()
tryAutoSetNumCapabilities (Logger logger) = do
  -- the current `+RTS -N` value:
  capabilitiesBefore <- getNumCapabilities
  processors <- getNumProcessors
  -- Abort if the user set capabilities explicitly. Note we can't detect
  -- where they intentionally set this to the number of processors.
  if capabilitiesBefore /= processors
    then
      logg LevelInfo
        $ "It looks like `+RTS -N` was passed explicitly. Leaving haskell capabilities at "
        <> showBlob capabilitiesBefore
    else handle (handler capabilitiesBefore) (setBasedOnCgroups capabilitiesBefore)
  where
    logg level = logger . UnstructuredLog level . SB.fromText

    showBlob :: (Show a, IsString b) => a -> b
    showBlob = fromString . show

    setBasedOnCgroups capabilitiesBefore = do
      detectCgroupVersion >>= \case
        Nothing ->
          logg LevelInfo
            $ "Could not detect cgroups. "
            <> leavingThingsAloneMsg capabilitiesBefore
        Just v -> do
          getCpuLimit v >>= \case
            InCgroupNoLimitSet ->
              logg LevelInfo
                $ "Running within a cgroup but no CPU limit detected. Leaving haskell capabilities at "
                <> showBlob capabilitiesBefore
            NotInCgroup ->
              logg LevelInfo
                $ "We don't appear to be running within a cgroup. Leaving haskell capabilities at "
                <> showBlob capabilitiesBefore
            InCgroupLimited cpuAllocation -> do
              processors <- getNumProcessors
              -- `floor` is also an options here, but rounding up just seems
              -- like the better/conservative choice here.
              -- Docker prevents setting cpuAllocation > processors, but I
              -- don't think that is disallowed in cgroups generally, so clamp
              -- here.
              let caps = min processors (ceiling cpuAllocation)
              setNumCapabilities caps
              logg LevelInfo
                $ "Detected "
                <> showBlob cpuAllocation
                <> " CPU cgroup limit. "
                <> "Setting number of haskell capabilities to: "
                <> showBlob caps

    getCpuLimit V1 = getCpuLimitV1
    getCpuLimit V2 = getCpuLimitV2

    leavingThingsAloneMsg capabilitiesBefore =
      "Leaving haskell capabilities at "
        <> showBlob capabilitiesBefore
        <> ". If running in docker or kubernetes with CPU limits, for best performance you should start the engine "
        <> "with `+RTS -N<cpu_limit>` on the command line, or `GHCRTS=-N<cpu_limit>` in the environment."

    handler :: Int -> SomeException -> IO ()
    handler capabilitiesBefore e =
      logg LevelWarn
        $ "Failed to automatically detect capabilities, with error:"
        <> showBlob e
        <> "\n"
        <> leavingThingsAloneMsg capabilitiesBefore
        <> "\n Please report this as a bug"

data CgroupVersion = V1 | V2 deriving (Eq, Show)

-- | Try to detect which version of cgroups the OS is using (if any?)
detectCgroupVersion :: IO (Maybe CgroupVersion)
detectCgroupVersion = do
  cgroupControllersExists <- doesFileExist "/sys/fs/cgroup/cgroup.controllers"
  cgroupDirExists <- doesDirectoryExist "/sys/fs/cgroup"
  return
    $ if cgroupControllersExists
      then Just V2
      else
        if cgroupDirExists
          then Just V1
          else Nothing

data CPULimits
  = -- | It looks like we're running in a cgroup but CPU is unlimited
    InCgroupNoLimitSet
  | -- | It doesn't look like the process is running in a cgroup
    NotInCgroup
  | -- | It looks like we're limited to N CPU (likely running in docker or k8s)
    InCgroupLimited Double
  deriving (Eq, Show)

-- | Lots of possible exceptions here
getCpuLimitV1 :: IO CPULimits
getCpuLimitV1 = do
  quotaExists <- doesFileExist "/sys/fs/cgroup/cpu/cpu.cfs_quota_us"
  periodExists <- doesFileExist "/sys/fs/cgroup/cpu/cpu.cfs_period_us"
  if quotaExists && periodExists
    then do
      quota <- readFile "/sys/fs/cgroup/cpu/cpu.cfs_quota_us"
      period <- readFile "/sys/fs/cgroup/cpu/cpu.cfs_period_us"
      if quota == "-1"
        then return InCgroupNoLimitSet
        else do
          let !limit = read quota / read period
          return $ InCgroupLimited limit
    else return NotInCgroup

-- | Lots of possible exceptions here
getCpuLimitV2 :: IO CPULimits
getCpuLimitV2 = do
  cpuMaxExists <- doesFileExist "/sys/fs/cgroup/cpu.max"
  if cpuMaxExists
    then do
      maxContent <- readFile "/sys/fs/cgroup/cpu.max"
      (quota, period) <- case words maxContent of
        [quota, period] -> return (quota, period)
        _ -> throwIO $ userError $ "Unexpected format for /sys/fs/cgroup/cpu.max: " <> show maxContent
      if quota == "max"
        then return InCgroupNoLimitSet
        else do
          let !limit = read quota / read period
          return $ InCgroupLimited limit
    else return NotInCgroup
