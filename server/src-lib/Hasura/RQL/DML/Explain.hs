{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hasura.RQL.DML.Explain where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH

import qualified Data.ByteString.Builder as BB

import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Select
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Data.String.Conversions as CS
import qualified Data.Text               as T
import qualified Database.PG.Query       as Q

data RQLExplain =
  RQLExplain
  { rqleQuery   :: !SelectQuery
  , rqleRole    :: !RoleName
  , rqleHeaders :: !HeaderObj
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 4 camelCase) ''RQLExplain)

data ExplainResp =
  ExplainResp
  { erSql   :: !T.Text
  , erPlans :: !Value
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 2 camelCase) ''ExplainResp)

phaseOneExplain :: SelectQuery -> P1 AnnSel
phaseOneExplain = convSelectQuery txtRHSBuilder

phaseTwoExplain :: (P2C m) => AnnSel -> m RespBody
phaseTwoExplain sel = do
  planResp <- liftTx $ runIdentity . Q.getRow <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder withExplain) [] True
  plans <- decodeBS planResp
  return $ encode $ ExplainResp selectSQLT plans
  where
    selectSQL = toSQL $ mkSQLSelect False sel
    explainSQL = BB.string7 "EXPLAIN (FORMAT JSON) "
    withExplain = explainSQL <> selectSQL

    decodeBS bs = case eitherDecode bs of
      Left e  -> throw500 $
        "Plan query response is invalid json; " <> T.pack e
      Right a -> return a

    selectSQLT = CS.cs $ BB.toLazyByteString selectSQL
