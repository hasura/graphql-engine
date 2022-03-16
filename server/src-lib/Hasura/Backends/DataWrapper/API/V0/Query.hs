--
module Hasura.Backends.DataWrapper.API.V0.Query
  ( Query (..),
    Field (..),
    ForeignKey (..),
    PrimaryKey (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict qualified as M
import Hasura.Backends.DataWrapper.API.V0.Column qualified as API.V0
import Hasura.Backends.DataWrapper.API.V0.Expression qualified as API.V0
import Hasura.Backends.DataWrapper.API.V0.OrderBy qualified as API.V0
import Hasura.Backends.DataWrapper.API.V0.Table qualified as API.V0
import Hasura.Prelude

--------------------------------------------------------------------------------

-- | A serializable request to retrieve strutured data from some
-- source.
data Query = Query
  { fields :: M.HashMap Text Field,
    from :: API.V0.TableName,
    limit :: Maybe Int,
    offset :: Maybe Int,
    where_ :: Maybe API.V0.Expression,
    orderBy :: [API.V0.OrderBy]
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

--------------------------------------------------------------------------------

-- | A serializable field targeted by a 'Query'.
data Field
  = ColumnField API.V0.ColumnName
  | RelationshipField (M.HashMap PrimaryKey ForeignKey) Query
  deriving stock (Eq, Ord, Show, Generic, Data)

--------------------------------------------------------------------------------

newtype PrimaryKey = PrimaryKey API.V0.ColumnName
  deriving stock (Data, Generic)
  deriving newtype (Eq, Hashable, Ord, Show, FromJSON, ToJSON)

--------------------------------------------------------------------------------

newtype ForeignKey = ForeignKey API.V0.ColumnName
  deriving stock (Data, Generic)
  deriving newtype (Eq, Hashable, Ord, Show, FromJSON, ToJSON)
