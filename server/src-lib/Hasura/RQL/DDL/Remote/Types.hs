-- |

module Hasura.RQL.DDL.Remote.Types where

import           Hasura.Prelude
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Set (Set)
import qualified Database.PG.Query as Q
import           Hasura.RQL.DDL.Remote.Input
import           Instances.TH.Lift ()
import qualified Language.GraphQL.Draft.Syntax as G
import           Language.Haskell.TH.Syntax (Lift)

data CreateRemoteRelationship =
  CreateRemoteRelationship
    { ccrName :: RemoteRelationshipName
    , ccrTable :: QualifiedTable
    , ccrRemoteSchema :: RemoteSchemaName
    , ccrRemoteField :: G.Name
    , ccrHasuraFields :: Set FieldName
    , ccrRemoteArguments :: RemoteArguments
    }
  deriving (Show, Eq, Lift)

newtype RemoteSchemaName
  = RemoteSchemaName
  { unRemoteSchemaName :: Text}
  deriving (Show, Eq, Lift, Hashable, ToJSON, ToJSONKey, FromJSON, Q.ToPrepArg, Q.FromCol)

newtype RemoteRelationshipName
  = RemoteRelationshipName
  { unRemoteRelationshipName :: Text}
  deriving (Show, Eq, Lift, Hashable, ToJSON, ToJSONKey, FromJSON, Q.ToPrepArg, Q.FromCol)

$(deriveJSON (aesonDrop 3 snakeCase) ''CreateRemoteRelationship)
