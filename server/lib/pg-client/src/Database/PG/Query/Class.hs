{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use onLeft" #-}

module Database.PG.Query.Class
  ( FromCol (..),
    fromColHelper,
    FromRes (..),
    ToPrepArg (..),
    toPrepValHelper,
    ToPrepArgs (..),
    SingleRow (..),
    Discard (..),
    ViaJSON (..),
    JSON (..),
    JSONB (..),
  )
where

-------------------------------------------------------------------------------

import Control.Monad ((>=>))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson (FromJSON, ToJSON, Value, encode, parseJSON)
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first)
import Data.ByteString (ByteString, uncons)
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Foldable (for_)
import Data.Hashable (Hashable)
import Data.Int (Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Lazy qualified as Lazy (Text)
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import Data.UUID (UUID)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.Word (Word64)
import Database.PG.Query.Connection
import Database.PG.Query.PTI qualified as PTI
import Database.PostgreSQL.LibPQ qualified as PQ
import PostgreSQL.Binary.Decoding qualified as PD
import PostgreSQL.Binary.Encoding qualified as PE
import Prelude

-------------------------------------------------------------------------------

newtype SingleRow a = SingleRow
  { getRow :: a
  }
  deriving stock (Eq, Show)

-- | Helper newtype to allow parsing JSON directly into a chosen type
newtype ViaJSON a = ViaJSON {getViaJSON :: a}

instance (FromJSON a) => FromCol (ViaJSON a) where
  fromCol =
    decodeJson >=> parse
    where
      parse :: Value -> Either Text (ViaJSON a)
      parse = fmap ViaJSON . first fromString . parseEither parseJSON

      decodeJson :: Maybe ByteString -> Either Text Value
      decodeJson = fromColHelper PD.json_ast . fmap dropFirst

      -- JSONB output starts with a ASCII SOH byte \x1
      -- if we find it, drop it, the rest should be valid JSON
      dropFirst :: ByteString -> ByteString
      dropFirst bs =
        case uncons bs of
          Just (bsHead, bsTail) ->
            if bsHead == 1
              then bsTail
              else bs
          Nothing -> bs

class FromCol a where
  fromCol ::
    Maybe ByteString ->
    Either Text a

fromColHelper :: PD.Value a -> Maybe ByteString -> Either Text a
fromColHelper f = maybe (throwError "encountered null") (PD.valueParser f)

instance FromCol Int where
  fromCol = fromColHelper PD.int

instance FromCol Int32 where
  fromCol = fromColHelper PD.int

instance FromCol Int64 where
  fromCol = fromColHelper PD.int

instance FromCol Int16 where
  fromCol = fromColHelper PD.int

instance FromCol Float where
  fromCol = fromColHelper PD.float4

instance FromCol Double where
  fromCol = fromColHelper PD.float8

instance FromCol Scientific where
  fromCol = fromColHelper PD.numeric

instance FromCol Char where
  fromCol = fromColHelper PD.char

instance FromCol Text where
  fromCol = fromColHelper PD.text_strict

instance FromCol Lazy.Text where
  fromCol = fromColHelper PD.text_lazy

instance FromCol ByteString where
  fromCol = fromColHelper PD.bytea_strict

instance FromCol Lazy.ByteString where
  fromCol = fromColHelper PD.bytea_lazy

instance FromCol Day where
  fromCol = fromColHelper PD.date

instance FromCol TimeOfDay where
  fromCol = fromColHelper PD.time_int

instance FromCol UTCTime where
  fromCol = fromColHelper PD.timestamptz_int

instance FromCol LocalTime where
  fromCol = fromColHelper PD.timestamp_int

instance FromCol Bool where
  fromCol = fromColHelper PD.bool

instance FromCol UUID where
  fromCol = fromColHelper PD.uuid

instance (FromCol a) => FromCol (Maybe a) where
  fromCol Nothing = return Nothing
  fromCol bs = Just <$> fromCol bs

class FromRes a where
  fromRes :: ResultOk -> ExceptT Text IO a

instance FromRes ResultOk where
  fromRes = return

instance FromRes () where
  fromRes (ResultOkEmpty _) =
    return ()
  fromRes (ResultOkData _) =
    throwError "Expecting no data for (). Instead, status is 'TuplesOk'"

instance FromRes Discard where
  fromRes (ResultOkEmpty _) =
    return $ Discard ()
  fromRes (ResultOkData _) =
    return $ Discard ()

newtype Discard = Discard ()
  deriving stock (Eq, Show)

type ResultMatrix = V.Vector ResultRow

type ResultRow = V.Vector (Maybe ByteString)

{-# INLINE colInt #-}
colInt :: PQ.Column -> Int
colInt (PQ.Col n) = fromIntegral n

{-# INLINE rowInt #-}
rowInt :: PQ.Row -> Int
rowInt (PQ.Row n) = fromIntegral n

-- Should be used only after checking
-- the status
buildMat :: PQ.Result -> IO ResultMatrix
buildMat r = do
  nr <- PQ.ntuples r
  nc <- PQ.nfields r
  mvx <- VM.unsafeNew (rowInt nr)
  for_ [0 .. pred nr] $ \ir -> do
    mvy <- VM.unsafeNew (colInt nc)
    for_ [0 .. pred nc] $ \ic ->
      VM.unsafeWrite mvy (colInt ic) =<< PQ.getvalue r ir ic
    vy <- V.unsafeFreeze mvy
    VM.unsafeWrite mvx (rowInt ir) vy
  V.unsafeFreeze mvx

instance (FromRow a) => FromRes [a] where
  fromRes (ResultOkEmpty _) =
    throwError "Expecting data. Instead, status is 'CommandOk'"
  fromRes (ResultOkData res) = do
    rm <- liftIO $ buildMat res
    ExceptT $ return $ fmap V.toList $ sequence $ V.map fromRow rm

instance (FromRow a) => FromRes (V.Vector a) where
  fromRes (ResultOkEmpty _) =
    throwError "Expecting data. Instead, status is 'CommandOk'"
  fromRes (ResultOkData res) = do
    rm <- liftIO $ buildMat res
    ExceptT $ return $ sequence $ V.map fromRow rm

instance (FromRow a) => FromRes (SingleRow a) where
  fromRes (ResultOkEmpty _) =
    throwError "Expecting data. Instead, status is 'CommandOk'"
  fromRes (ResultOkData res) = do
    rm <- liftIO $ buildMat res
    if V.length rm == 1
      then ExceptT $ return $ SingleRow <$> fromRow (rm V.! 0)
      else throwError "Rows returned != 1"

instance (FromRow a) => FromRes (Maybe a) where
  fromRes (ResultOkEmpty _) =
    throwError "Expecting data. Instead, status is 'CommandOk'"
  fromRes (ResultOkData res) = do
    rm <- liftIO $ buildMat res
    case V.length rm of
      0 -> ExceptT $ return $ Right Nothing
      1 -> ExceptT $ return $ Just <$> fromRow (rm V.! 0)
      _ -> throwError "Rows returned > 1"

colMismatch :: Int -> Int -> Text
colMismatch expected actual =
  fromString $
    mconcat
      [ "Expected ",
        show expected,
        " column(s), but found: ",
        show actual
      ]

instance (FromCol a) => FromRow (Identity a) where
  fromRow row = case V.length row of
    1 -> fmap Identity $ fromCol $ row V.! 0
    c -> throwError $ colMismatch 1 c

instance (FromCol a, FromCol b) => FromRow (a, b) where
  fromRow row = case V.length row of
    2 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      return (a, b)
    c -> throwError $ colMismatch 2 c

instance (FromCol a, FromCol b, FromCol c) => FromRow (a, b, c) where
  fromRow row = case V.length row of
    3 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      c <- fromCol $ row V.! 2
      return (a, b, c)
    c -> throwError $ colMismatch 3 c

instance (FromCol a, FromCol b, FromCol c, FromCol d) => FromRow (a, b, c, d) where
  fromRow row = case V.length row of
    4 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      c <- fromCol $ row V.! 2
      d <- fromCol $ row V.! 3
      return (a, b, c, d)
    c -> throwError $ colMismatch 4 c

instance (FromCol a, FromCol b, FromCol c, FromCol d, FromCol e) => FromRow (a, b, c, d, e) where
  fromRow row = case V.length row of
    5 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      c <- fromCol $ row V.! 2
      d <- fromCol $ row V.! 3
      e <- fromCol $ row V.! 4
      return (a, b, c, d, e)
    c -> throwError $ colMismatch 5 c

instance (FromCol a, FromCol b, FromCol c, FromCol d, FromCol e, FromCol f) => FromRow (a, b, c, d, e, f) where
  fromRow row = case V.length row of
    6 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      c <- fromCol $ row V.! 2
      d <- fromCol $ row V.! 3
      e <- fromCol $ row V.! 4
      f <- fromCol $ row V.! 5
      return (a, b, c, d, e, f)
    c -> throwError $ colMismatch 6 c

instance
  (FromCol a, FromCol b, FromCol c, FromCol d, FromCol e, FromCol f, FromCol g) =>
  FromRow (a, b, c, d, e, f, g)
  where
  fromRow row = case V.length row of
    7 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      c <- fromCol $ row V.! 2
      d <- fromCol $ row V.! 3
      e <- fromCol $ row V.! 4
      f <- fromCol $ row V.! 5
      g <- fromCol $ row V.! 6
      return (a, b, c, d, e, f, g)
    c -> throwError $ colMismatch 7 c

instance
  (FromCol a, FromCol b, FromCol c, FromCol d, FromCol e, FromCol f, FromCol g, FromCol h) =>
  FromRow (a, b, c, d, e, f, g, h)
  where
  fromRow row = case V.length row of
    8 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      c <- fromCol $ row V.! 2
      d <- fromCol $ row V.! 3
      e <- fromCol $ row V.! 4
      f <- fromCol $ row V.! 5
      g <- fromCol $ row V.! 6
      h <- fromCol $ row V.! 7
      return (a, b, c, d, e, f, g, h)
    c -> throwError $ colMismatch 8 c

instance
  (FromCol a, FromCol b, FromCol c, FromCol d, FromCol e, FromCol f, FromCol g, FromCol h, FromCol i) =>
  FromRow (a, b, c, d, e, f, g, h, i)
  where
  fromRow row = case V.length row of
    9 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      c <- fromCol $ row V.! 2
      d <- fromCol $ row V.! 3
      e <- fromCol $ row V.! 4
      f <- fromCol $ row V.! 5
      g <- fromCol $ row V.! 6
      h <- fromCol $ row V.! 7
      i <- fromCol $ row V.! 8
      return (a, b, c, d, e, f, g, h, i)
    c -> throwError $ colMismatch 9 c

instance
  (FromCol a, FromCol b, FromCol c, FromCol d, FromCol e, FromCol f, FromCol g, FromCol h, FromCol i, FromCol j) =>
  FromRow (a, b, c, d, e, f, g, h, i, j)
  where
  fromRow row = case V.length row of
    10 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      c <- fromCol $ row V.! 2
      d <- fromCol $ row V.! 3
      e <- fromCol $ row V.! 4
      f <- fromCol $ row V.! 5
      g <- fromCol $ row V.! 6
      h <- fromCol $ row V.! 7
      i <- fromCol $ row V.! 8
      j <- fromCol $ row V.! 9
      return (a, b, c, d, e, f, g, h, i, j)
    c -> throwError $ colMismatch 10 c

instance
  (FromCol a, FromCol b, FromCol c, FromCol d, FromCol e, FromCol f, FromCol g, FromCol h, FromCol i, FromCol j, FromCol k) =>
  FromRow (a, b, c, d, e, f, g, h, i, j, k)
  where
  fromRow row = case V.length row of
    11 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      c <- fromCol $ row V.! 2
      d <- fromCol $ row V.! 3
      e <- fromCol $ row V.! 4
      f <- fromCol $ row V.! 5
      g <- fromCol $ row V.! 6
      h <- fromCol $ row V.! 7
      i <- fromCol $ row V.! 8
      j <- fromCol $ row V.! 9
      k <- fromCol $ row V.! 10
      return (a, b, c, d, e, f, g, h, i, j, k)
    c -> throwError $ colMismatch 11 c

instance
  (FromCol a, FromCol b, FromCol c, FromCol d, FromCol e, FromCol f, FromCol g, FromCol h, FromCol i, FromCol j, FromCol k, FromCol l) =>
  FromRow (a, b, c, d, e, f, g, h, i, j, k, l)
  where
  fromRow row = case V.length row of
    12 -> do
      a <- fromCol $ row V.! 0
      b <- fromCol $ row V.! 1
      c <- fromCol $ row V.! 2
      d <- fromCol $ row V.! 3
      e <- fromCol $ row V.! 4
      f <- fromCol $ row V.! 5
      g <- fromCol $ row V.! 6
      h <- fromCol $ row V.! 7
      i <- fromCol $ row V.! 8
      j <- fromCol $ row V.! 9
      k <- fromCol $ row V.! 10
      l <- fromCol $ row V.! 11
      return (a, b, c, d, e, f, g, h, i, j, k, l)
    c -> throwError $ colMismatch 12 c

class FromRow a where
  fromRow :: ResultRow -> Either Text a

class ToPrepArgs a where
  toPrepArgs :: a -> [PrepArg]

instance ToPrepArgs () where
  toPrepArgs _ = []

class ToPrepArg a where
  toPrepVal :: a -> PrepArg

instance ToPrepArg PrepArg where
  toPrepVal arg = arg

toPrepValHelper :: PQ.Oid -> (a -> PE.Encoding) -> a -> PrepArg
toPrepValHelper o e a = (o, Just (PE.encodingBytes $ e a, PQ.Binary))

instance (ToJSON a) => ToPrepArg (ViaJSON a) where
  toPrepVal (ViaJSON a) = toPrepValHelper PTI.json PE.bytea_lazy $ encode a

instance ToPrepArg Word64 where
  toPrepVal = toPrepValHelper PTI.int8 PE.int8_word64

instance ToPrepArg Int64 where
  toPrepVal = toPrepValHelper PTI.int8 PE.int8_int64

instance ToPrepArg Int32 where
  toPrepVal = toPrepValHelper PTI.int4 PE.int4_int32

instance ToPrepArg Int16 where
  toPrepVal = toPrepValHelper PTI.int2 PE.int2_int16

instance ToPrepArg Float where
  toPrepVal = toPrepValHelper PTI.float4 PE.float4

instance ToPrepArg Double where
  toPrepVal = toPrepValHelper PTI.float8 PE.float8

instance ToPrepArg Scientific where
  toPrepVal = toPrepValHelper PTI.numeric PE.numeric

instance ToPrepArg Char where
  toPrepVal = toPrepValHelper PTI.text PE.char_utf8

instance ToPrepArg Text where
  toPrepVal = toPrepValHelper PTI.text PE.text_strict

instance ToPrepArg Lazy.Text where
  toPrepVal = toPrepValHelper PTI.text PE.text_lazy

instance ToPrepArg ByteString where
  toPrepVal = toPrepValHelper PTI.bytea PE.bytea_strict

instance ToPrepArg Lazy.ByteString where
  toPrepVal = toPrepValHelper PTI.bytea PE.bytea_lazy

instance ToPrepArg LocalTime where
  toPrepVal = toPrepValHelper PTI.timestamp PE.timestamp_int

instance ToPrepArg UTCTime where
  toPrepVal = toPrepValHelper PTI.timestamptz PE.timestamptz_int

instance ToPrepArg Bool where
  toPrepVal = toPrepValHelper PTI.bool PE.bool

instance ToPrepArg Day where
  toPrepVal = toPrepValHelper PTI.date PE.date

instance ToPrepArg UUID where
  toPrepVal = toPrepValHelper PTI.uuid PE.uuid

newtype JSON = JSON Value
  deriving stock (Eq, Show)
  deriving newtype (Hashable)

newtype JSONB = JSONB Value
  deriving stock (Eq, Show)
  deriving newtype (Hashable)

instance ToPrepArg JSON where
  toPrepVal (JSON j) = toPrepValHelper PTI.json PE.json_ast j

instance ToPrepArg JSONB where
  toPrepVal (JSONB j) = toPrepValHelper PTI.jsonb PE.jsonb_ast j

instance (ToPrepArg a) => ToPrepArg (Maybe a) where
  toPrepVal (Just a) = toPrepVal a
  -- FIX ME, the oid here should be particular to the type
  toPrepVal Nothing = (PTI.auto, Nothing)

instance (ToPrepArg a) => ToPrepArgs [a] where
  toPrepArgs = map toPrepVal

instance (ToPrepArg a) => ToPrepArgs (Identity a) where
  toPrepArgs (Identity a) =
    [toPrepVal a]

instance (ToPrepArg a, ToPrepArg b) => ToPrepArgs (a, b) where
  toPrepArgs (a, b) =
    [ toPrepVal a,
      toPrepVal b
    ]

instance (ToPrepArg a, ToPrepArg b, ToPrepArg c) => ToPrepArgs (a, b, c) where
  toPrepArgs (a, b, c) =
    [ toPrepVal a,
      toPrepVal b,
      toPrepVal c
    ]

instance (ToPrepArg a, ToPrepArg b, ToPrepArg c, ToPrepArg d) => ToPrepArgs (a, b, c, d) where
  toPrepArgs (a, b, c, d) =
    [ toPrepVal a,
      toPrepVal b,
      toPrepVal c,
      toPrepVal d
    ]

instance (ToPrepArg a, ToPrepArg b, ToPrepArg c, ToPrepArg d, ToPrepArg e) => ToPrepArgs (a, b, c, d, e) where
  toPrepArgs (a, b, c, d, e) =
    [ toPrepVal a,
      toPrepVal b,
      toPrepVal c,
      toPrepVal d,
      toPrepVal e
    ]

instance (ToPrepArg a, ToPrepArg b, ToPrepArg c, ToPrepArg d, ToPrepArg e, ToPrepArg f) => ToPrepArgs (a, b, c, d, e, f) where
  toPrepArgs (a, b, c, d, e, f) =
    [ toPrepVal a,
      toPrepVal b,
      toPrepVal c,
      toPrepVal d,
      toPrepVal e,
      toPrepVal f
    ]

instance (ToPrepArg a, ToPrepArg b, ToPrepArg c, ToPrepArg d, ToPrepArg e, ToPrepArg f, ToPrepArg g) => ToPrepArgs (a, b, c, d, e, f, g) where
  toPrepArgs (a, b, c, d, e, f, g) =
    [ toPrepVal a,
      toPrepVal b,
      toPrepVal c,
      toPrepVal d,
      toPrepVal e,
      toPrepVal f,
      toPrepVal g
    ]

instance
  (ToPrepArg a, ToPrepArg b, ToPrepArg c, ToPrepArg d, ToPrepArg e, ToPrepArg f, ToPrepArg g, ToPrepArg h) =>
  ToPrepArgs (a, b, c, d, e, f, g, h)
  where
  toPrepArgs (a, b, c, d, e, f, g, h) =
    [ toPrepVal a,
      toPrepVal b,
      toPrepVal c,
      toPrepVal d,
      toPrepVal e,
      toPrepVal f,
      toPrepVal g,
      toPrepVal h
    ]

instance
  (ToPrepArg a, ToPrepArg b, ToPrepArg c, ToPrepArg d, ToPrepArg e, ToPrepArg f, ToPrepArg g, ToPrepArg h, ToPrepArg i) =>
  ToPrepArgs (a, b, c, d, e, f, g, h, i)
  where
  toPrepArgs (a, b, c, d, e, f, g, h, i) =
    [ toPrepVal a,
      toPrepVal b,
      toPrepVal c,
      toPrepVal d,
      toPrepVal e,
      toPrepVal f,
      toPrepVal g,
      toPrepVal h,
      toPrepVal i
    ]

instance
  (ToPrepArg a, ToPrepArg b, ToPrepArg c, ToPrepArg d, ToPrepArg e, ToPrepArg f, ToPrepArg g, ToPrepArg h, ToPrepArg i, ToPrepArg j) =>
  ToPrepArgs (a, b, c, d, e, f, g, h, i, j)
  where
  toPrepArgs (a, b, c, d, e, f, g, h, i, j) =
    [ toPrepVal a,
      toPrepVal b,
      toPrepVal c,
      toPrepVal d,
      toPrepVal e,
      toPrepVal f,
      toPrepVal g,
      toPrepVal h,
      toPrepVal i,
      toPrepVal j
    ]
