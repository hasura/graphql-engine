module Hasura.RQL.IR.RemoteJoin
  ( RemoteJoin(..)
  ) where

import           Hasura.Prelude

import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.RQL.IR.Select
import           Hasura.RQL.Types


-- | A 'RemoteJoin' represents the context of remote relationship to be extracted from 'AnnFieldG's.
data RemoteJoin (b :: BackendType)
  = RemoteJoin
  { _rjName          :: !FieldName -- ^ The remote join field name.
  , _rjArgs          :: ![RemoteFieldArgument] -- ^ User-provided arguments with variables.
  , _rjSelSet        :: !(G.SelectionSet G.NoFragments RemoteSchemaVariable)  -- ^ User-provided selection set of remote field.
  , _rjHasuraFields  :: !(HashSet FieldName) -- ^ Table fields.
  , _rjFieldCall     :: !(NonEmpty FieldCall) -- ^ Remote server fields.
  , _rjRemoteSchema  :: !RemoteSchemaInfo -- ^ The remote schema server info.
  , _rjPhantomFields :: ![ColumnInfo b]
    -- ^ Hasura fields which are not in the selection set, but are required as
    -- parameters to satisfy the remote join.
  }
deriving instance Backend b => Eq (RemoteJoin b)
