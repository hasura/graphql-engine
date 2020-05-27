module Hasura.RQL.DML.Returning.Types where


import           Hasura.Prelude

import qualified Data.Text                   as T
import qualified Hasura.SQL.DML              as S

import           Hasura.RQL.DML.Select.Types


data MutFldG v
  = MCount
  | MExp !T.Text
  | MRet !(AnnFldsG v)
  deriving (Show, Eq)

type MutFld = MutFldG S.SQLExp

type MutFldsG v = Fields (MutFldG v)

data MutationOutputG v
  = MOutMultirowFields !(MutFldsG v)
  | MOutSinglerowObject !(AnnFldsG v)
  deriving (Show, Eq)

type MutationOutput = MutationOutputG S.SQLExp

type MutFlds = MutFldsG S.SQLExp
