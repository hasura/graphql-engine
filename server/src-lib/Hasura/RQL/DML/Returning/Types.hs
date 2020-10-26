module Hasura.RQL.DML.Returning.Types where

import           Hasura.SQL.Backend

import           Hasura.Prelude

import qualified Data.Aeson                  as J
import qualified Data.HashMap.Strict.InsOrd  as OMap
import qualified Data.Text                   as T
import qualified Hasura.SQL.DML              as S

import           Hasura.EncJSON
import           Hasura.RQL.DML.Select.Types


data MutFldG (b :: Backend) v
  = MCount
  | MExp !T.Text
  | MRet !(AnnFieldsG b v)

type MutFld b = MutFldG b S.SQLExp

type MutFldsG b v = Fields (MutFldG b v)

data MutationOutputG (b :: Backend) v
  = MOutMultirowFields !(MutFldsG b v)
  | MOutSinglerowObject !(AnnFieldsG b v)

type MutationOutput b = MutationOutputG b S.SQLExp

type MutFlds b = MutFldsG b S.SQLExp

buildEmptyMutResp :: MutationOutput backend -> EncJSON
buildEmptyMutResp = \case
  MOutMultirowFields mutFlds -> encJFromJValue $ OMap.fromList $ map (second convMutFld) mutFlds
  MOutSinglerowObject _      -> encJFromJValue $ J.Object mempty
  where
    convMutFld = \case
      MCount -> J.toJSON (0 :: Int)
      MExp e -> J.toJSON e
      MRet _ -> J.toJSON ([] :: [J.Value])
