{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Hasura.RQL.DML.QueryTemplate where

import           Hasura.Prelude
import           Hasura.RQL.DDL.QueryTemplate
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Returning     (encodeJSONVector)
import           Hasura.RQL.GBoolExp          (txtRHSBuilder)
import           Hasura.RQL.Instances         ()
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Database.PG.Query            as Q
import qualified Hasura.RQL.DML.Count         as R
import qualified Hasura.RQL.DML.Delete        as R
import qualified Hasura.RQL.DML.Insert        as R
import qualified Hasura.RQL.DML.Select        as R
import qualified Hasura.RQL.DML.Update        as R
import qualified Hasura.SQL.DML               as S

import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Instances.TH.Lift            ()
import           Language.Haskell.TH.Syntax   (Lift)

import qualified Data.ByteString.Builder      as BB
import qualified Data.HashMap.Strict          as M
import qualified Data.Sequence                as DS
import qualified Data.Vector                  as V

type TemplateArgs = M.HashMap TemplateParam Value

data ExecQueryTemplate
  = ExecQueryTemplate
  { eqtName :: !TQueryName
  , eqtArgs :: !TemplateArgs
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''ExecQueryTemplate)

type EQTP1 = StateT (DS.Seq Q.PrepArg) P1

getParamValue
  :: TemplateArgs
  -> TemplateParamConf
  -> EQTP1 Value
getParamValue params (TemplateParamConf paramName paramVal) =
  maybe paramMissing return $ M.lookup paramName params <|> paramVal
  where
    paramMissing = throw400 InvalidParams $
      "missing parameter : " <>> paramName

data QueryTProc
  = QTPInsert !(R.InsertQueryP1, DS.Seq Q.PrepArg)
  | QTPSelect !(R.AnnSel, DS.Seq Q.PrepArg)
  | QTPUpdate !(R.UpdateQueryP1, DS.Seq Q.PrepArg)
  | QTPDelete !(R.DeleteQueryP1, DS.Seq Q.PrepArg)
  | QTPCount !(R.CountQueryP1, DS.Seq Q.PrepArg)
  | QTPBulk ![QueryTProc]
  deriving (Show, Eq)

buildPrepArg
  :: TemplateArgs
  -> PGColType
  -> Value
  -> EQTP1 S.SQLExp
buildPrepArg args pct val =
  case val of
    Object _ -> do
      tpc <- decodeValue val
      v <- getParamValue args tpc
      modifyErr (withParamErrMsg tpc) $ binRHSBuilder pct v
    _ -> txtRHSBuilder pct val
  where
    withParamErrMsg tpc t =
      "when processing parameter " <> tpcParam tpc  <<> " : " <> t

decodeIntValue :: TemplateArgs -> Value -> EQTP1 Int
decodeIntValue args val =
  case val of
   Object _ -> do
     tpc <- decodeValue val
     v <- getParamValue args tpc
     decodeValue v
   _        -> decodeValue val

mkSelQWithArgs :: SelectQueryT -> TemplateArgs -> EQTP1 SelectQuery
mkSelQWithArgs (DMLQuery tn (SelectG c w o lim offset)) args = do
  intLim <- mapM (decodeIntValue args) lim
  intOffset <- mapM (decodeIntValue args) offset
  return $ DMLQuery tn $ SelectG c w o intLim intOffset

convQT
  :: (P1C m)
  => TemplateArgs
  -> QueryT
  -> m QueryTProc
convQT args qt = case qt of
  QTInsert q -> fmap QTPInsert $ peelSt $
                R.convInsertQuery decodeParam binRHSBuilder q
  QTSelect q -> fmap QTPSelect $ peelSt $
                mkSelQWithArgs q args >>= R.convSelectQuery f
  QTUpdate q -> fmap QTPUpdate $ peelSt $ R.convUpdateQuery f q
  QTDelete q -> fmap QTPDelete $ peelSt $ R.convDeleteQuery f q
  QTCount q  -> fmap QTPCount $ peelSt $ R.countP1 f q
  QTBulk q   -> fmap QTPBulk $ mapM (convQT args) q
  where
    decodeParam val = do
      tpc <- decodeValue val
      v <- getParamValue args tpc
      R.decodeInsObjs v

    f = buildPrepArg args
    peelSt m = do
      sc <- askSchemaCache
      ui <- askUserInfo
      liftEither $ runP1 (QCtx ui sc) $ runStateT m DS.empty

execQueryTemplateP1 :: ExecQueryTemplate -> P1 QueryTProc
execQueryTemplateP1 (ExecQueryTemplate qtn args) = do
  (QueryTemplateInfo _ qt _) <- askQTemplateInfo qtn
  convQT args qt

execQueryTP2 :: (P2C m) => QueryTProc -> m RespBody
execQueryTP2 qtProc = case qtProc of
  QTPInsert qp -> liftTx $ R.insertP2 qp
  QTPSelect qp -> liftTx $ R.selectP2 False qp
  QTPUpdate qp -> liftTx $ R.updateP2 qp
  QTPDelete qp -> liftTx $ R.deleteP2 qp
  QTPCount qp  -> R.countP2 qp
  QTPBulk qps  -> do
    respList <- mapM execQueryTP2 qps
    let bsVector = V.fromList respList
    return $ BB.toLazyByteString $ encodeJSONVector BB.lazyByteString bsVector

instance HDBQuery ExecQueryTemplate where

  type Phase1Res ExecQueryTemplate = QueryTProc
  phaseOne = execQueryTemplateP1

  phaseTwo _ = execQueryTP2

  schemaCachePolicy = SCPNoChange
