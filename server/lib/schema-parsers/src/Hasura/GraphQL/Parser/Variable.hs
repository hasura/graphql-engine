module Hasura.GraphQL.Parser.Variable
  ( InputValue (..),
    Variable (..),
    VariableInfo (..),
  )
where

import Data.Aeson qualified as J
import Data.Hashable (Hashable)
import Data.Void (Void)
import GHC.Generics (Generic)
import Hasura.GraphQL.Parser.Names
import Language.GraphQL.Draft.Syntax
  ( GType (..),
    Name (..),
    Value (..),
  )
import Language.Haskell.TH.Lift qualified as TH

{- Note [Parsing variable values]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GraphQL includes its own tiny language for input values, which is similar to
JSON but not quite the same---GraphQL input values can be enum values, and there
are restrictions on the names of input object keys. Despite these differences,
variables’ values are passed as JSON, so we actually need to be able to parse
values expressed in both languages.

It’s tempting to contain this complexity by simply converting the JSON values to
GraphQL input values up front, and for booleans, numbers, arrays, and most
objects, this conversion is viable. But JSON strings pose a problem, since they
are used to represent both GraphQL strings and GraphQL enums. For example,
consider a query like this:

    enum FooBar {
      FOO
      BAR
    }

    query some_query($a: String, $b: FooBar) {
      ...
    }

We might receive an accompany variables payload like this:

    {
      "a": "FOO",
      "b": "FOO"
    }

To properly convert these JSON values to GraphQL, we’d need to use the type
information to guide the parsing. Since $a has type String, its value should be
parsed as the GraphQL string "FOO", while $b has type FooBar, so its value
should be parsed as the GraphQL enum value FOO.

We could do this type-directed parsing, but there are some advantages to being
lazier. For one, we can use JSON values directly when used as a column value of
type json or jsonb, rather than converting them to GraphQL and back; which, in
turn, solves another problem with JSON objects: JSON object keys are arbitrary
strings, while GraphQL input object keys are GraphQL names, and therefore
restricted: not all JSON objects can be represented by a GraphQL input object.

Arguably such columns should really be represented as strings containing encoded
JSON, not GraphQL lists/objects, but the decision to treat them otherwise is
old, and it would be backwards-incompatible to change now. We can also avoid
needing to interpret the values of variables for types outside our control
(i.e. those from a remote schema), which can be useful in the case of custom
scalars or extensions of the GraphQL protocol.

So instead we use the InputValue type to represent that an input value might be
a GraphQL literal value or a JSON value from the variables payload. This means
each input parser constructor needs to be able to parse both GraphQL values and
JSON values, but fortunately, the duplication of logic is minimal. -}

-- | See Note [Parsing variable values].
data InputValue v
  = GraphQLValue (Value v)
  | JSONValue J.Value
  deriving (Show, Eq, Functor, Generic, Ord, TH.Lift)

instance (Hashable v) => Hashable (InputValue v)

data Variable = Variable
  { vInfo :: VariableInfo,
    vType :: GType,
    -- | The following cases are distinguished:
    --
    -- 1. A JSON value (including `null`) was provided: Just (JSONValue ...)
    -- 2. No JSON value was provided, but a default value exists: Just (GraphQLValue ...)
    -- 3. No JSON value was provided, and no default value exists: Nothing
    vValue :: Maybe (InputValue Void)
  }
  deriving (Show, Eq, Generic, Ord, TH.Lift)

instance Hashable Variable

instance HasName Variable where
  getName = getName . vInfo

data VariableInfo
  = VIRequired Name
  | -- | Unlike fields (see 'InputFieldInfo'), nullable variables with no
    -- default value are indistinguishable from variables with a default value
    -- of null, so we don’t distinguish those cases here.
    VIOptional Name (Value Void)
  deriving (Show, Eq, Generic, Ord, TH.Lift)

instance Hashable VariableInfo

instance HasName VariableInfo where
  getName (VIRequired name) = name
  getName (VIOptional name _) = name
