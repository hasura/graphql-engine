module Hasura.Backends.DataConnector.IR.Relationships
  ( RelationshipName,
    mkRelationshipName,
    TableRelationships (..),
    Relationship (..),
    RelationshipType (..),
    SourceColumnName,
    TargetColumnName,
  )
where

import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Extended (toTxt)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.IR.Column qualified as IR.C
import Hasura.Backends.DataConnector.IR.Name qualified as IR.N
import Hasura.Backends.DataConnector.IR.Table qualified as IR.T
import Hasura.Prelude
import Hasura.RQL.Types.Common (RelName (..))
import Witch qualified

type RelationshipName = IR.N.Name 'IR.N.Relationship

mkRelationshipName :: RelName -> RelationshipName
mkRelationshipName relName = IR.N.Name @('IR.N.Relationship) $ toTxt relName

type SourceTableName = IR.T.Name

newtype TableRelationships = TableRelationships
  {unTableRelationships :: HashMap SourceTableName (HashMap RelationshipName Relationship)}
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving newtype (ToJSON)

instance Semigroup TableRelationships where
  (TableRelationships l) <> (TableRelationships r) = TableRelationships $ HashMap.unionWith HashMap.union l r

instance Monoid TableRelationships where
  mempty = TableRelationships mempty

data Relationship = Relationship
  { _rTargetTable :: IR.T.Name,
    _rRelationshipType :: RelationshipType,
    _rColumnMapping :: HashMap SourceColumnName TargetColumnName
  }
  deriving stock (Data, Eq, Generic, Ord, Show)

instance ToJSON Relationship where
  toJSON = J.genericToJSON J.defaultOptions

instance Witch.From Relationship API.Relationship where
  from Relationship {..} =
    API.Relationship
      { _rTargetTable = Witch.from _rTargetTable,
        _rRelationshipType = Witch.from _rRelationshipType,
        _rColumnMapping = HashMap.mapKeys Witch.from $ Witch.from <$> _rColumnMapping
      }

instance Witch.From API.Relationship Relationship where
  from API.Relationship {..} =
    Relationship
      { _rTargetTable = Witch.from _rTargetTable,
        _rRelationshipType = Witch.from _rRelationshipType,
        _rColumnMapping = HashMap.mapKeys Witch.from $ Witch.from <$> _rColumnMapping
      }

data RelationshipType = ObjectRelationship | ArrayRelationship
  deriving stock (Eq, Ord, Show, Generic, Data)

instance ToJSON RelationshipType where
  toJSON = J.genericToJSON J.defaultOptions

instance Witch.From RelationshipType API.RelationshipType where
  from = \case
    ObjectRelationship -> API.ObjectRelationship
    ArrayRelationship -> API.ArrayRelationship

instance Witch.From API.RelationshipType RelationshipType where
  from = \case
    API.ObjectRelationship -> ObjectRelationship
    API.ArrayRelationship -> ArrayRelationship

type SourceColumnName = IR.C.Name

type TargetColumnName = IR.C.Name
