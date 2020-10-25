module Hasura.RQL.IR.RemoteJoin
  ( RemoteJoins
  , RemoteJoinMap
  , Alias
  , FieldPath(..)
  , RemoteJoin(..)
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict           as Map
import qualified Data.List.NonEmpty            as NE
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Parser         hiding (field)
import           Hasura.RQL.IR.Select
import           Hasura.RQL.Types


-- | A 'RemoteJoin' represents the context of remote relationship to be extracted from 'AnnFieldG's.
data RemoteJoin (b :: Backend)
  = RemoteJoin
  { _rjName          :: !FieldName -- ^ The remote join field name.
  , _rjArgs          :: ![RemoteFieldArgument] -- ^ User-provided arguments with variables.
  , _rjSelSet        :: !(G.SelectionSet G.NoFragments Variable)  -- ^ User-provided selection set of remote field.
  , _rjHasuraFields  :: !(HashSet FieldName) -- ^ Table fields.
  , _rjFieldCall     :: !(NonEmpty FieldCall) -- ^ Remote server fields.
  , _rjRemoteSchema  :: !RemoteSchemaInfo -- ^ The remote schema server info.
  , _rjPhantomFields :: ![ColumnInfo b]
    -- ^ Hasura fields which are not in the selection set, but are required as
    -- parameters to satisfy the remote join.
  }
deriving instance Eq (RemoteJoin 'Postgres)

-- | Path to the remote join field in query response JSON from Postgres.
newtype FieldPath = FieldPath {unFieldPath :: [FieldName]}
  deriving (Show, Eq, Semigroup, Monoid, Hashable)

type Alias = G.Name

type RemoteJoins b = NE.NonEmpty (FieldPath, NE.NonEmpty (RemoteJoin b))
type RemoteJoinMap b = Map.HashMap FieldPath (NE.NonEmpty (RemoteJoin b))
