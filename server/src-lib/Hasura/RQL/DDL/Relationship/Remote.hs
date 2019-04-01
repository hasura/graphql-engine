module Hasura.RQL.DDL.Relationship.Remote where

import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Instances.TH.Lift             ()
import           Language.Haskell.TH.Syntax    (Lift)

import           Hasura.RQL.DDL.Relationship

import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G

data RemoteRelUsing
  = RemoteRelUsing
  { rruTable       :: !QualifiedTable
  , rruColumn      :: !PGCol
  , rruRemoteField :: !G.Name
  , rruInputField  :: !T.Text
  , rruInputPath   :: !(Maybe T.Text)
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''RemoteRelUsing)

type RemoteRelDef = RelDef RemoteRelUsing

type CreateRemoteRel = WithTable RemoteRelDef

runCreateRemoteRel
  :: (QErrM m, CacheRWM m, MonadTx m , UserInfoM m)
  => CreateRemoteRel -> m EncJSON
runCreateRemoteRel defn = do
  createRemoteRelP1 defn
  createRemoteRelP2 defn


createRemoteRelP1 :: (UserInfoM m, QErrM m, CacheRM m) => CreateRemoteRel -> m ()
createRemoteRelP1 (WithTable qt rd) = do
  adminOnly
  validateRemoteRel qt rd

validateRemoteRel
  :: (QErrM m, CacheRM m)
  => QualifiedTable -> RemoteRelDef -> m ()
validateRemoteRel qt (RelDef rn ru _) = undefined

createRemoteRelP2
  :: (QErrM m, CacheRWM m, MonadTx m) => CreateRemoteRel -> m EncJSON
createRemoteRelP2 (WithTable qt rd) = do
  remoteRelP2 qt rd
  return successMsg

remoteRelP2
  :: (QErrM m, CacheRWM m, MonadTx m)
  => QualifiedTable -> RemoteRelDef -> m ()
remoteRelP2 qt rd@(RelDef rn u comment) = do
  remoteRelP2Setup qt rd
  liftTx $ persistRel qt rn RemoteRel (toJSON u) comment

remoteRelP2Setup
  :: (QErrM m, CacheRWM m, MonadTx m)
  => QualifiedTable -> RemoteRelDef -> m ()
remoteRelP2Setup qt (RelDef rn ru _) = undefined








