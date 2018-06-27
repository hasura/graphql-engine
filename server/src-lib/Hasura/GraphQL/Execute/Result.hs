{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasura.GraphQL.Execute.Result
  ( encodeGQErr
  , encodeJSONObject
  , encodeGQResp
  , mkJSONObj
  , GQResp(..)
  ) where

import           Hasura.Prelude

import qualified Data.Aeson              as J
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Text.Encoding      as TE
import qualified Data.Vector             as V

import           Hasura.RQL.Types

encodeGQErr :: Text -> QErr -> J.Value
encodeGQErr role qErr =
  J.object [ "errors" J..= [encodeQErr role qErr]]

data GQResp
  = GQSuccess BL.ByteString
  | GQPreExecError [J.Value]
  | GQExecError [J.Value]
  deriving (Show, Eq)

encodeJSONObject :: V.Vector (Text, BL.ByteString) -> BB.Builder
encodeJSONObject xs
  | V.null xs = BB.char7 '{' <> BB.char7 '}'
  | otherwise = BB.char7 '{' <> builder' (V.unsafeHead xs) <>
                V.foldr go (BB.char7 '}') (V.unsafeTail xs)
  where
    go v b  = BB.char7 ',' <> builder' v <> b
    -- builds "key":value from (key,value)
    builder' (t, v) =
      BB.char7 '"' <> TE.encodeUtf8Builder t <> BB.string7 "\":"
      <> BB.lazyByteString v

encodeGQResp :: GQResp -> BL.ByteString
encodeGQResp gqResp =
  buildBS $ case gqResp of
    GQSuccess r      -> V.singleton ("data", r)
    GQPreExecError e -> V.singleton ("errors", J.encode e)
    GQExecError e    -> V.fromList [("data", "null"), ("errors", J.encode e)]
  where
    buildBS = BB.toLazyByteString . encodeJSONObject

mkJSONObj :: V.Vector (Text, BL.ByteString) -> BL.ByteString
mkJSONObj = BB.toLazyByteString . encodeJSONObject
