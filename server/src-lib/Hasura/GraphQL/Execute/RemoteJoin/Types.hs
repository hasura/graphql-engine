module Hasura.GraphQL.Execute.RemoteJoin.Types
  ( FieldPath(..)
  , appendPath
  , RemoteJoin(..)
  , RemoteJoins
  , RemoteJoinMap
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict           as Map
import qualified Data.List.NonEmpty            as NE
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.RQL.IR.Select
import           Hasura.RQL.Types

-- | Path to the remote join field in query response JSON from Postgres.
newtype FieldPath = FieldPath {unFieldPath :: [FieldName]}
  deriving (Show, Eq, Semigroup, Monoid, Hashable)

appendPath :: FieldName -> FieldPath -> FieldPath
appendPath fieldName = FieldPath . (<> [fieldName]) . unFieldPath

{- Note [Phantom fields in Remote Joins]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Usually, a join appears when we establish a relationship between two entities.
In this case, a remote join is a relationship between a table from a database of
any source to a remote GraphQL schema. The relationship is defined as native
fields (column or scalar computed field) acts as input argument for a field in the
remote schema. In order to make a call to remote schema we need to have values of
the table fields. The fields may or may not be present in the actual selection set.
If they aren't present, we need to fetch them explicitly from the database by inserting
them in the generated SQL statement. Thus they're called phantom fields. In post-fetching
joining process we need to remove the phantom fields and send the response to client.

Limitation for scalar computed fields:
--------------------------------------
We need to ensure a scalar computed field is to be included as phantom field if not
present in the query selection set. But if the SQL function associated with the scalar
computed field has input arguments other than table row and hasura session inputs, we
cannot determine their values. Hence, we only accept scalar computed fields with no
input arguments except table row and hasura session arguments in forming remote relationships.

Example: Let's say we have a computed field 'calculate_something' whose SQL function accepts
input argument 'xfactor: Integer' other than table row input and hasura session argument.
A remote relationship 'something_from_calculated' is defined and included in query selection set.
The 'calculate_something' is absent in the selection set, so we need to include it in the phantom
fields. Now, what's the value we should consider for the 'xfactor' input argument? So, we do
restrict these scalar computed fields in forming the remote relationships at metadata API level.

Solution:
--------
A potential solution for aforementioned limitation is to update the create_remote_relationship
metadata API to accept the computed fields with values of input arguments other than table row
and hasura session arguments.
-}

-- | A 'RemoteJoin' represents the context of remote relationship to be extracted from 'AnnFieldG's.
data RemoteJoin
  = RemoteJoin
  { _rjName          :: !FieldName -- ^ The remote join field name.
  , _rjArgs          :: ![RemoteFieldArgument] -- ^ User-provided arguments with variables.
  , _rjSelSet        :: !(G.SelectionSet G.NoFragments RemoteSchemaVariable)  -- ^ User-provided selection set of remote field.
  , _rjHasuraFields  :: !(HashSet FieldName) -- ^ Table fields.
  , _rjFieldCall     :: !(NonEmpty FieldCall) -- ^ Remote server fields.
  , _rjRemoteSchema  :: !RemoteSchemaInfo -- ^ The remote schema server info.
  , _rjPhantomFields :: ![FieldName]
    -- ^ Hasura fields which are not in the selection set, but are required as
    -- parameters to satisfy the remote join. See Note [Phantom fields in Remote Joins].
  } deriving (Eq)

type RemoteJoins = NE.NonEmpty (FieldPath, NE.NonEmpty RemoteJoin)
type RemoteJoinMap = Map.HashMap FieldPath (NE.NonEmpty RemoteJoin)
