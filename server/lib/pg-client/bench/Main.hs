{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main,
  )
where

-------------------------------------------------------------------------------

import Control.Exception qualified as E
import Control.Monad (unless)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson qualified as J
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.FileEmbed qualified as FE
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Int (Int64)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Database.PG.Query qualified as PG
import Hasql.Decoders qualified as HD
import Hasql.Encoders qualified as HE
import Hasql.Pool qualified as HP
import Hasql.Statement qualified as HS
import Hasql.Transaction qualified as HT
import Hasql.Transaction.Sessions qualified as HT
import System.IO.Error qualified as E
import Test.Tasty.Bench qualified as C
import Prelude

-------------------------------------------------------------------------------

withEx :: (Show e) => IO (Either e a) -> IO a
withEx action =
  action >>= either (E.throwIO . E.userError . show) return

runCTx :: PG.PGPool -> PG.TxE PG.PGExecErr a -> IO a
runCTx pool tx =
  withEx $ runExceptT $ PG.runTx pool (PG.Serializable, Just PG.ReadWrite) tx

runHTx :: HP.Pool -> HT.Transaction a -> IO a
runHTx pool tx =
  withEx $ HP.use pool $ HT.transaction HT.Serializable HT.Write tx

type CTx a = PG.TxE PG.PGExecErr a

type HTx = HT.Transaction

benchQ ::
  (PG.PGPool, HP.Pool) ->
  String ->
  (Bool -> CTx B.ByteString, Bool -> HTx B.ByteString) ->
  IO C.Benchmark
benchQ (poolC, poolH) n (txC, txH) = do
  resC <- runCTx poolC $ txC False
  resCP <- runCTx poolC $ txC True
  resH <- runHTx poolH $ txH False
  resHP <- runHTx poolH $ txH True

  unless (resC == resCP && resCP == resH && resH == resHP) $ do
    BC.putStrLn $ "pg-client: " <> resC
    BC.putStrLn $ "pg-client-prepared: " <> resCP
    BC.putStrLn $ "hasql" <> resH
    BC.putStrLn $ "hasql-prepared" <> resHP
    E.throwIO $ E.userError $ "results are not the same for: " <> n

  return $
    C.bgroup
      n
      [ C.bench "pg-client" $ C.whnfIO $ runCTx poolC $ txC False,
        C.bench "pg-client-prepared" $ C.whnfIO $ runCTx poolC $ txC True,
        C.bench "hasql" $ C.whnfIO $ runHTx poolH $ txH False,
        C.bench "hasql-prepared" $ C.whnfIO $ runHTx poolH $ txH True
      ]

getPoolC :: IO PG.PGPool
getPoolC = do
  let connDetails =
        PG.CDOptions
          PG.ConnOptions
            { PG.connHost = "127.0.0.1",
              PG.connPort = 7432,
              PG.connUser = "admin",
              PG.connPassword = "",
              PG.connDatabase = "chinook",
              PG.connOptions = Nothing
            }
      connInfo = PG.ConnInfo 0 connDetails
      connParams = PG.ConnParams 1 1 180 False Nothing Nothing False
      logger = const (return ())
  PG.initPGPool connInfo J.Null connParams logger

q1 :: T.Text
q1 = $(FE.embedStringFile "bench/queries/artistByArtistId.sql")

mkTx1C :: Bool -> CTx B.ByteString
mkTx1C isPrepared =
  runIdentity . PG.getRow
    <$> PG.withQE
      PG.PGExecErrTx
      (PG.fromText q1)
      (Identity (3 :: Int64))
      isPrepared

mkTx1H :: Bool -> HTx B.ByteString
mkTx1H isPrepared =
  HT.statement 3 $ HS.Statement (TE.encodeUtf8 q1) encoder decoder isPrepared
  where
    encoder = HE.param $ HE.nonNullable HE.int8
    decoder = HD.singleRow $ HD.column $ HD.nonNullable $ HD.custom $ \_ bs -> return bs

q2 :: T.Text
q2 = $(FE.embedStringFile "bench/queries/allArtists.sql")

mkTx2C :: Bool -> CTx B.ByteString
mkTx2C isPrepared =
  runIdentity . PG.getRow
    <$> PG.withQE
      PG.PGExecErrTx
      (PG.fromText q2)
      ()
      isPrepared

mkTx2H :: Bool -> HTx B.ByteString
mkTx2H isPrepared =
  HT.statement () $ HS.Statement (TE.encodeUtf8 q2) encoder decoder isPrepared
  where
    encoder = HE.noParams
    decoder = HD.singleRow $ HD.column $ HD.nonNullable $ HD.custom $ \_ bs -> return bs

main :: IO ()
main = do
  poolC <- getPoolC
  poolH <- HP.acquire (1, 180, "postgresql://admin@127.0.0.1:7432/chinook")

  benchmarks <-
    sequence
      [ benchQ (poolC, poolH) "artistByArtistId" (mkTx1C, mkTx1H),
        benchQ (poolC, poolH) "allArtists" (mkTx2C, mkTx2H)
      ]

  C.defaultMain benchmarks

  PG.destroyPGPool poolC
  HP.release poolH
