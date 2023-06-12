module Hasura.Server.ResourceCheckerSpec (spec) where

import Hasura.Prelude
import Hasura.Server.ResourceChecker qualified as RC
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Test.Hspec qualified as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Resource Checker Tests" $ do
  resourceCheckerSpec

resourceCheckerSpec :: Hspec.Spec
resourceCheckerSpec =
  Hspec.describe "get container resource allocations" $ do
    Hspec.it "get current container resources" $ do
      sr <- RC.getServerResources
      RC._rcrCpu sr `Hspec.shouldSatisfy` (\mc -> ((>= 0) <$> mc) == (Just True))
      RC._rcrMemory sr `Hspec.shouldSatisfy` (\mm -> ((>= 0) <$> mm) == (Just True))

    Hspec.it "test cgroup v1 allocations successfully" $ do
      cgroupRoot <- createMockCGroupV1 (Just 100000) (Just 100000) (Just 1024) (Just "536870912")
      result <- RC.getCGroupV1Resources cgroupRoot
      result `Hspec.shouldBe` (RC.ComputeResourcesResponse (Just 1024) (Just 536870912) Nothing)

    Hspec.it "test cgroup v1 allocations with inconclusive mem" $ do
      cgroupRoot <- createMockCGroupV1 (Just 100000) (Just 100000) (Just 1024) Nothing
      result <- RC.getCGroupV1Resources cgroupRoot
      maxMemory <- RC.getMaxPhysicalMemory
      result `Hspec.shouldBe` (RC.ComputeResourcesResponse (Just 1024) maxMemory (Just RC.MemoryInconclusive))

    Hspec.it "test cgroup v1 allocations with inconclusive cpu" $ do
      cgroupRoot <- createMockCGroupV1 Nothing Nothing Nothing (Just "536870912")
      result <- RC.getCGroupV1Resources cgroupRoot
      maxCpu <- RC.getPhysicalCpuResource
      result `Hspec.shouldBe` (RC.ComputeResourcesResponse (Just maxCpu) (Just 536870912) (Just RC.CpuInconclusive))

    Hspec.it "test cgroup v2 allocations with the empty folder" $ do
      cgroupRoot <- getCanonicalTemporaryDirectory
      result <- RC.getCGroupV2Resources cgroupRoot
      expected <- RC.getPhysicalResources (Just RC.CpuMemoryInconclusive)
      result `Hspec.shouldBe` expected

    Hspec.it "test cgroup v2 allocations successfully" $ do
      cgroupRoot <- createMockCGroupV2 (Just 100000) (Just 100000) (Just 1024) (Just "536870912")
      result <- RC.getCGroupV2Resources cgroupRoot
      result `Hspec.shouldBe` (RC.ComputeResourcesResponse (Just 1024) (Just 536870912) Nothing)

    Hspec.it "test cgroup v2 max memory allocation" $ do
      cgroupRoot <- createMockCGroupV2 (Just 100000) (Just 100000) (Just 1024) (Just "max")
      result <- RC.getCGroupV2Resources cgroupRoot
      maxMemory <- RC.getMaxPhysicalMemory
      result `Hspec.shouldBe` (RC.ComputeResourcesResponse (Just 1024) maxMemory Nothing)

    Hspec.it "test cgroup v2 allocations with inconclusive mem" $ do
      cgroupRoot <- createMockCGroupV2 (Just 100000) (Just 100000) (Just 1024) Nothing
      result <- RC.getCGroupV2Resources cgroupRoot
      maxMemory <- RC.getMaxPhysicalMemory
      result `Hspec.shouldBe` (RC.ComputeResourcesResponse (Just 1024) maxMemory (Just RC.MemoryInconclusive))

    Hspec.it "test cgroup v2 allocations with extreme high memory" $ do
      cgroupRoot <- createMockCGroupV2 (Just 100000) (Just 100000) (Just 1024) (Just "9223372036854772000")
      result <- RC.getCGroupV2Resources cgroupRoot
      maxMemory <- RC.getMaxPhysicalMemory
      result `Hspec.shouldBe` (RC.ComputeResourcesResponse (Just 1024) maxMemory (Just RC.MemoryInconclusive))

    Hspec.it "test cgroup v2 allocations with inconclusive cpu" $ do
      cgroupRoot <- createMockCGroupV2 Nothing Nothing Nothing (Just "536870912")
      result <- RC.getCGroupV2Resources cgroupRoot
      maxCpu <- RC.getPhysicalCpuResource
      result `Hspec.shouldBe` (RC.ComputeResourcesResponse (Just maxCpu) (Just 536870912) (Just RC.CpuInconclusive))

    Hspec.it "test cgroup v2 allocations with the empty folder" $ do
      cgroupRoot <- getCanonicalTemporaryDirectory
      result <- RC.getCGroupV2Resources cgroupRoot
      expected <- RC.getPhysicalResources (Just RC.CpuMemoryInconclusive)
      result `Hspec.shouldBe` expected

    Hspec.it "determine cgroup v1 container resources from mount info" $ do
      cgroupRoot <- createMockCGroupV1 (Just (-1)) (Just 100000) (Just 2) (Just "140918784")
      mountPath <- createMountFile cgroupRoot (mockCGroup1MountContent cgroupRoot)
      result <- RC.getServerResources_ mountPath
      maxCpu <- RC.getPhysicalCpuResource
      result `Hspec.shouldBe` (RC.ComputeResourcesResponse (Just maxCpu) (Just 140918784) (Just RC.CpuInconclusive))

    Hspec.it "determine cgroup v2 container resources from mount info" $ do
      cgroupRoot <- createMockCGroupV2 (Just 100000) (Just 100000) (Just 1024) (Just "536870912")
      mountPath <- createMountFile cgroupRoot (mockCGroup1MountContent cgroupRoot)
      createCGroupV2ControllersFile cgroupRoot
      result <- RC.getServerResources_ mountPath
      result `Hspec.shouldBe` (RC.ComputeResourcesResponse (Just 1024) (Just 536870912) Nothing)

    Hspec.it "determine hybrid container resources from mount info" $ do
      cgroupRoot <- createMockCGroupV1 (Just 50000) (Just 100000) (Just 512) (Just "402653184")
      mountPath <- createMountFile cgroupRoot (mockCGroup2MountContent cgroupRoot)
      result <- RC.getServerResources_ mountPath
      result `Hspec.shouldBe` (RC.ComputeResourcesResponse (Just 512) (Just 402653184) Nothing)

    Hspec.it "determine fallback resources" $ do
      cgroupRoot <- createMockCGroupV1 Nothing Nothing (Just 2048) (Just "536870912")
      mountPath <- createMountFile cgroupRoot (mockCGroup2MountContent cgroupRoot)
      createCGroupV2ControllersFile cgroupRoot
      result <- RC.getServerResources_ mountPath
      result `Hspec.shouldBe` (RC.ComputeResourcesResponse (Just 2048) (Just 536870912) Nothing)

createMockCGroupV1 :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe String -> IO FilePath
createMockCGroupV1 mQuota mPeriod mShares mMemoryBytes = do
  cgroupRoot <- getCanonicalTemporaryDirectory >>= flip createTempDirectory "cgroup"
  let cpuRoot = cgroupRoot </> "cpu"
      memoryRoot = cgroupRoot </> "memory"
      files =
        [ ("cpu.shares", mShares),
          ("cpu.cfs_quota_us", mQuota),
          ("cpu.cfs_period_us", mPeriod)
        ]
  mapM_ createDirectory [cpuRoot, memoryRoot]
  mapM_ (\(name, mContent) -> for_ mContent (writeFile (cpuRoot </> name) . show)) files
  for_ mMemoryBytes (writeFile (memoryRoot </> "memory.limit_in_bytes"))
  return cgroupRoot

createMockCGroupV2 :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe String -> IO FilePath
createMockCGroupV2 mQuota mPeriod mRawShares mMemoryBytes = do
  cgroupRoot <- getCanonicalTemporaryDirectory >>= flip createTempDirectory "cgroup2"
  let mShares = mRawShares <&> (\rawShares -> show $ 1 + ((rawShares - 2) * 9999) `div` 262142)
      files =
        [ ("cpu.weight", mShares),
          ("cpu.max", mCpuMax),
          ("memory.max", mMemoryBytes)
        ]
  writeFile (cgroupRoot </> "cgroup.controllers") "cpu memory"
  mapM_ (\(name, mContent) -> for_ mContent (writeFile (cgroupRoot </> name))) files
  return cgroupRoot
  where
    cpuMaxWords = filter (/= "") ((maybe "" show) <$> [mQuota, mPeriod])
    mCpuMax = bool (Just $ unwords cpuMaxWords) Nothing (null cpuMaxWords)

createMountFile :: FilePath -> String -> IO FilePath
createMountFile dir content = do
  let path = (dir </> "mounts")
  writeFile path content
  return path

createCGroupV2ControllersFile :: FilePath -> IO ()
createCGroupV2ControllersFile dir =
  writeFile (dir </> "cgroup.controllers") "cpuset cpu io memory hugetlb pids rdma misc"

mockCGroup1MountContent :: String -> String
mockCGroup1MountContent root =
  "1259 1235 0:239 / /proc rw,nosuid,nodev,noexec,relatime - proc proc rw\n"
    ++ "1260 1235 0:240 / /dev rw,nosuid - tmpfs tmpfs rw,size=65536k,mode=755\n"
    ++ "1261 1260 0:241 / /dev/pts rw,nosuid,noexec,relatime - devpts devpts rw,gid=5,mode=620,ptmxmode=666\n"
    ++ "1263 1262 0:242 / /sys/fs/cgroup rw,nosuid,nodev,noexec,relatime - tmpfs tmpfs rw,mode=755\n"
    ++ ("1264 1263 0:25 /kubepods/burstable/pod37349393 " ++ root ++ "/systemd ro,nosuid,nodev,noexec,relatime master:9 - cgroup cgroup rw,xattr\n")
    ++ ("1265 1263 0:27 /kubepods/burstable/pod37349393 " ++ root ++ "/pids ro,nosuid,nodev,noexec,relatime master:10 - cgroup cgroup rw,pids")

mockCGroup2MountContent :: String -> String
mockCGroup2MountContent root =
  "28 22 0:24 / /dev/pts rw,nosuid,noexec,relatime shared:4 - devpts devpts rw,gid=5,mode=620,ptmxmode=000\n"
    ++ ("29 21 0:25 / " ++ root ++ " rw,nosuid,nodev,noexec,relatime shared:9 - cgroup2 cgroup2 rw,nsdelegate,memory_recursiveprot\n")
    ++ "30 21 0:26 / /sys/fs/pstore rw,nosuid,nodev,noexec,relatime shared:10 - pstore pstore rw\n"
    ++ "31 21 0:27 / /sys/fs/bpf rw,nosuid,nodev,noexec,relatime shared:11 - bpf bpf rw,mode=700"
