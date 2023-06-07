module Main (main) where

import qualified Test.Framework as Test
import Database.Redis
import Tests
import PubSubTest

main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    Test.defaultMain (tests conn)

tests :: Connection -> [Test.Test]
tests conn = map ($ conn) $ concat
    [ testsMisc, testsKeys, testsStrings, [testHashes], testsLists, testsSets, [testHyperLogLog]
    , testsZSets, [testPubSub], [testTransaction], [testScripting]
    , testsConnection, testsServer, [testScans, testSScan, testHScan, testZScan], [testZrangelex]
    , [testXAddRead, testXReadGroup, testXRange, testXpending, testXClaim, testXInfo, testXDel, testXTrim]
    , testPubSubThreaded
      -- should always be run last as connection gets closed after it
    , [testQuit]
    ]

testsServer :: [Test]
testsServer =
    [testServer, testBgrewriteaof, testFlushall, testInfo, testConfig
    ,testSlowlog, testDebugObject]

testsConnection :: [Test]
testsConnection = [ testConnectAuth, testConnectAuthUnexpected, testConnectDb
                  , testConnectDbUnexisting, testEcho, testPing, testSelect ]

testsKeys :: [Test]
testsKeys = [ testKeys, testKeysNoncluster, testExpireAt, testSort, testGetType, testObject ]
