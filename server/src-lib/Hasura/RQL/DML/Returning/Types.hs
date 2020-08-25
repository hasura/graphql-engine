module Hasura.RQL.DML.Returning.Types where


import           Hasura.Prelude

import qualified Data.Aeson                  as J
import qualified Data.HashMap.Strict.InsOrd  as OMap
import qualified Data.Text                   as T
import qualified Hasura.SQL.DML              as S

import           Hasura.EncJSON
import           Hasura.RQL.DML.Select.Types


data MutFldG v
  = MCount
  | MExp !T.Text
  | MRet !(AnnFieldsG v)
  deriving (Show, Eq)

type MutFld = MutFldG S.SQLExp

type MutFldsG v = Fields (MutFldG v)

data MutationOutputG v
  = MOutMultirowFields !(MutFldsG v)
  | MOutSinglerowObject !(AnnFieldsG v)
  deriving (Show, Eq)

type MutationOutput = MutationOutputG S.SQLExp

type MutFlds = MutFldsG S.SQLExp

buildEmptyMutResp :: MutationOutput -> EncJSON
buildEmptyMutResp = \case
  MOutMultirowFields mutFlds -> encJFromJValue $ OMap.fromList $ map (second convMutFld) mutFlds
  MOutSinglerowObject _      -> encJFromJValue $ J.Object mempty
  where
    convMutFld = \case
      MCount -> J.toJSON (0 :: Int)
      MExp e -> J.toJSON e
      MRet _ -> J.toJSON ([] :: [J.Value])
