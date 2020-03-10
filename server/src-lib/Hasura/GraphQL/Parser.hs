{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}

module Hasura.GraphQL.Parser where

import Hasura.Prelude

import qualified Data.HashMap.Strict.Extended as M
import qualified Data.HashSet as S

import Language.GraphQL.Draft.Syntax (Description(..), EnumValue(..), Name(..), SelectionSet, Selection(..), Value(..), Field(..), literal, litName, mkName, unsafeMkName)
import Data.Int (Int32)
import Control.Lens.Extended hiding (enum, index)
import Data.Type.Equality
import Data.Void
import Data.Parser.JSONPath
import Control.Monad.Validate

import qualified Hasura.RQL.Types.Column as RQL

import Hasura.SQL.DML
import Hasura.SQL.Types
import Hasura.SQL.Value
import Hasura.Server.Utils (englishList)
import Hasura.RQL.Types.Column hiding (EnumValue(..), EnumValueInfo(..))
import Hasura.RQL.Types.Permission (SessVar)
-- import Hasura.RQL.Types hiding (EnumValue(..), EnumValueInfo(..), FieldInfo(..))
-- import Hasura.GraphQL.Resolve.Types (UnresolvedVal(..), AnnPGVal(..))
-- import Hasura.GraphQL.Schema.Common

-- -------------------------------------------------------------------------------------------------

class Monad m => MonadParse m where
  withPath :: (JSONPath -> JSONPath) -> m a -> m a
  parseError :: Text -> m a

  -- | See 'QueryReusability'.
  markNotReusable :: m ()

newtype ParseT m a = ParseT
  { unParseT :: ReaderT ParseContext (StateT ParseState (ValidateT ParseError m)) a }
  deriving (Functor, Applicative, Monad)

newtype ParseContext = ParseContext
  { pcPath :: JSONPath }

newtype ParseState = ParseState
  { psReusability :: QueryReusability }

-- | Tracks whether or not a query is /reusable/. Reusable queries are nice,
-- since we can cache their resolved ASTs and avoid re-resolving them if we
-- receive an identical query. However, we can’t always safely reuse queries if
-- they have variables, since some variable values can affect the generated SQL.
-- For example, consider the following query:
--
-- > query users_where($condition: users_bool_exp!) {
-- >   users(where: $condition) {
-- >     id
-- >   }
-- > }
--
-- Different values for @$condition@ will produce completely different queries,
-- so we can’t reuse its plan (unless the variable values were also all
-- identical, of course, but we don’t bother caching those).
data QueryReusability = Reusable | NotReusable

data ParseError = ParseError
  { pePath :: JSONPath
  , peMessage :: Text
  }

-- -------------------------------------------------------------------------------------------------

-- | GraphQL types are divided into two classes: input types and output types.
-- The GraphQL spec does not use the word “kind” to describe these classes, but
-- it’s an apt term.
--
-- Some GraphQL types can be used at either kind, so we also include the 'Both'
-- kind, the superkind of both 'Input' and 'Output'. The '<:' class provides
-- kind subsumption constraints.
--
-- For more details, see <http://spec.graphql.org/June2018/#sec-Input-and-Output-Types>.
data Kind = Both | Input | Output

{- Note [The delicate balance of GraphQL kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The way we track kinds is rather delicate, and it succeeds many failed attempts
at encoding the same ideas. We rely on a happy coincidence to keep the types as
simple as possible: whether or not a field has a sub-selection set is knowable
from its kind alone!

  * Normal (non-input) object fields cannot have types of kind Input, so we can
    ignore that case.

  * The only types of kind Both are scalars and enums, neither of which accept a
    sub-selection set.

  * The remaining types, which we give kind Output, are objects, interfaces, and
    unions. We don’t currently support the latter two, so we only have to deal
    with objects, which always take sub-selection sets.

This allows us to conveniently re-use `Parser`s for types of kind Both to
represent output types for selection set fields (while stile disallowing types
of kind Input). This trick avoids the need to track that information separately,
significantly simplifying the types! But if that happy coincidence ever ceases
to hold, we’d have to rethink things. -}

-- | Evidence for '<:'.
data k1 :<: k2 where
  KRefl :: k :<: k
  KBoth :: k :<: 'Both

-- | 'Kind' subsumption. The GraphQL kind hierarchy is extremely simple:
--
-- >     Both
-- >     /  \
-- > Input  Output
--
-- Various functions in this module use '<:' to allow 'Both' to be used in
-- places where 'Input' or 'Output' would otherwise be expected.
class k1 <: k2 where
  subKind :: k1 :<: k2
instance k1 ~ k2 => k1 <: k2 where
  subKind = KRefl
instance {-# OVERLAPPING #-} k <: 'Both where
  subKind = KBoth

data Type k
  = NonNullable (NonNullableType k)
  | Nullable (NonNullableType k)

data NonNullableType k
  = TNamed (Definition (TypeInfo k))
  | TList (Type k)

discardNullability :: Type k -> NonNullableType k
discardNullability (NonNullable t) = t
discardNullability (Nullable t) = t

data TypeInfo k where
  TIScalar :: TypeInfo 'Both
  TIEnum :: NonEmpty (Definition EnumValueInfo) -> TypeInfo 'Both
  TIInputObject :: [Definition (FieldInfo 'Input)] -> TypeInfo 'Input
  TIObject :: [Definition (FieldInfo 'Output)] -> TypeInfo 'Output

data Definition a = Definition
  { dName :: Name
  , dDescription :: Maybe Description
  , dInfo :: ~a
  -- ^ Lazy to allow mutually-recursive type definitions without needing to
  -- eagerly construct an infinitely-large type definition.
  }

-- | Enum values have no extra information except for the information common to
-- all definitions, so this is just a placeholder for use as @'Definition'
-- 'EnumValueInfo'@.
data EnumValueInfo = EnumValueInfo

data family FieldInfo (k :: Kind)

data instance FieldInfo 'Input
  -- | A required field with a non-nullable type.
  = forall k. ('Input <: k) => IFRequired (NonNullableType k)
  -- | An optional input field with a nullable type and possibly a default
  -- value. If a default value is provided, it should be a valid value for the
  -- type.
  --
  -- Note that a default value of 'TNull' is subtly different from having no
  -- default value at all. If no default value is provided, the GraphQL
  -- specification allows distinguishing provided @null@ values from values left
  -- completely absent; see <http://spec.graphql.org/June2018/#CoerceArgumentValues()>.
  | forall k. ('Input <: k) => IFOptional (NonNullableType k) (Maybe (Value Void))

data instance FieldInfo 'Output = forall k. ('Output <: k) => FieldInfo
  { fArguments :: [Definition (FieldInfo 'Input)]
  , fType :: Type k
  }

data Variable = Variable
  { vDefinition :: Definition (FieldInfo 'Input)
  , vValue :: Value Void
  -- ^ Note: if the variable was null or was not provided and the field has a
  -- non-null default value, this field contains the default value, not 'VNull'.
  }

data Parser k m a = Parser
  { pType :: Type k
  , pParser :: ParserInput k -> m a
  } deriving (Functor)

type family ParserInput k where
  ParserInput 'Both = Value Variable
  ParserInput 'Input = Value Variable
  ParserInput 'Output = SelectionSet Variable

-- | The constraint @(''Input' '<:' k)@ entails @('ParserInput' k ~ 'Value')@,
-- but GHC can’t figure that out on its own, so we have to be explicit to give
-- it a little help.
inputParserInput :: forall k. 'Input <: k => ParserInput k :~: Value Variable
inputParserInput = case subKind @'Input @k of { KRefl -> Refl; KBoth -> Refl }

pInputParser :: forall k m a. 'Input <: k => Parser k m a -> Value Variable -> m a
pInputParser = gcastWith (inputParserInput @k) pParser

infixl 1 `bind`
bind :: Monad m => Parser k m a -> (a -> m b) -> Parser k m b
bind p f = p { pParser = pParser p >=> f }

-- | Parses some collection of input fields. Build a 'FieldsParser' using
-- 'field', 'fieldWithDefault', or 'fieldOptional', combine several together
-- with the 'Applicative' instance, and finish it off using 'object' to turn it
-- into a 'Parser'.
data FieldsParser k m a = FieldsParser
  -- Note: this is isomorphic to
  -- `Compose ((,) [Definition (FieldInfo k)]) (ReaderT (HashMap Name (FieldInput k)) m) a`,
  -- but working with that type kind of sucks.
  { ifDefinitions :: [Definition (FieldInfo k)]
  , ifParser :: HashMap Name (FieldInput k) -> m a
  } deriving (Functor)

type family FieldInput k = r | r -> k where
  FieldInput 'Input = Value Variable
  FieldInput 'Output = Field Variable

instance Applicative m => Applicative (FieldsParser k m) where
  pure v = FieldsParser [] (const $ pure v)
  a <*> b = FieldsParser
    (ifDefinitions a <> ifDefinitions b)
    (liftA2 (<*>) (ifParser a) (ifParser b))

peelVariable :: MonadParse m => Value Variable -> m (Value Variable)
peelVariable (VVariable (Variable { vValue })) = markNotReusable $> literal vValue
peelVariable value = pure value

data ScalarRepresentation a where
  SRBoolean :: ScalarRepresentation Bool
  SRInt :: ScalarRepresentation Int32
  SRFloat :: ScalarRepresentation Double
  SRString :: ScalarRepresentation Text

scalar
  :: MonadParse m
  => Name
  -> Maybe Description
  -> ScalarRepresentation a
  -> Parser 'Both m a
scalar name description representation = Parser
  { pType = NonNullable $ TNamed Definition
    { dName = name
    , dDescription = description
    , dInfo = TIScalar
    }
  , pParser = peelVariable >=> \v -> case representation of
      SRBoolean -> case v of
        VBoolean a -> pure a
        _ -> typeMismatch name "a boolean" v
      SRInt -> case v of
        VInt a -> pure a
        _ -> typeMismatch name "an integer" v
      SRFloat -> case v of
        VFloat a -> pure a
        _ -> typeMismatch name "a float" v
      SRString -> case v of
        VString a -> pure a
        _ -> typeMismatch name "a string" v
  }

boolean :: MonadParse m => Parser 'Both m Bool
boolean = scalar $$(litName "Boolean") Nothing SRBoolean

int :: MonadParse m => Parser 'Both m Int32
int = scalar $$(litName "Int") Nothing SRInt

float :: MonadParse m => Parser 'Both m Double
float = scalar $$(litName "Float") Nothing SRFloat

string :: MonadParse m => Parser 'Both m Text
string = scalar $$(litName "String") Nothing SRString

enum
  :: MonadParse m
  => Name
  -> Maybe Description
  -> NonEmpty (Definition EnumValueInfo, a)
  -> Parser 'Both m a
enum name description values = Parser
  { pType = NonNullable $ TNamed Definition
    { dName = name
    , dDescription = description
    , dInfo = TIEnum (fst <$> values)
    }
  , pParser = peelVariable >=> \case
      VEnum (EnumValue value) -> case M.lookup value valuesMap of
        Just result -> pure result
        Nothing -> parseError $ "expected one of the values "
          <> englishList "or" (dquoteTxt . dName . fst <$> values) <> "for type "
          <> name <<> ", but found " <>> value
      other -> typeMismatch name "an enum value" other
  }
  where
    valuesMap = M.fromList $ over (traverse._1) dName $ toList values

nullable :: forall k m a. (MonadParse m, 'Input <: k) => Parser k m a -> Parser k m (Maybe a)
nullable parser = gcastWith (inputParserInput @k) Parser
  { pType = case pType parser of
      NonNullable t -> Nullable t
      Nullable t -> Nullable t
  , pParser = peelVariable >=> \case
      VNull -> pure Nothing
      other -> Just <$> pParser parser other
  }

list :: forall k m a. (MonadParse m, 'Input <: k) => Parser k m a -> Parser k m [a]
list parser = gcastWith (inputParserInput @k) Parser
  { pType = NonNullable $ TList $ pType parser
  , pParser = peelVariable >=> \case
      VList values -> for (zip [0..] values) \(index, value) ->
        withPath (Index index :) $ pParser parser value
      other -> parseError $ "expected a list, but found " <> describeValue other
  }

object
  :: MonadParse m
  => Name
  -> Maybe Description
  -> FieldsParser 'Input m a
  -> Parser 'Input m a
object name description parser = Parser
  { pType = NonNullable $ TNamed Definition
    { dName = name
    , dDescription = description
    , dInfo = TIInputObject (ifDefinitions parser)
    }
  , pParser = peelVariable >=> \case
      VObject fields -> do
        -- check for extraneous fields here, since the FieldsParser just
        -- handles parsing the fields it cares about
        for_ (M.keys fields) \fieldName -> do
          unless (fieldName `S.member` fieldNames) $
            parseError $ name <<> " has no field named " <>> fieldName
        ifParser parser fields
      other -> typeMismatch name "an object" other
  }
  where
    fieldNames = S.fromList (dName <$> ifDefinitions parser)

field
  :: (MonadParse m, 'Input <: k)
  => Name
  -> Maybe Description
  -> Parser k m a
  -> FieldsParser 'Input m a
field name description parser = case pType parser of
  NonNullable _ -> FieldsParser
    { ifDefinitions = [Definition
      { dName = name
      , dDescription = description
      , dInfo = case pType parser of
          NonNullable typ -> IFRequired typ
          Nullable typ -> IFOptional typ (Just VNull)
      }]
    , ifParser = M.lookup name
        >>> (`onNothing` parseError ("missing required field " <>> name))
        >=> withPath (Key (unName name) :) . pInputParser parser
    }
  -- nullable fields just have an implicit default value of `null`
  Nullable _ -> fieldWithDefault name description VNull parser

fieldWithDefault
  :: (MonadParse m, 'Input <: k)
  => Name
  -> Maybe Description
  -> Value Void -- ^ default value
  -> Parser k m a
  -> FieldsParser 'Input m a
fieldWithDefault name description defaultValue parser = FieldsParser
  { ifDefinitions = [Definition
    { dName = name
    , dDescription = description
    , dInfo = IFOptional (discardNullability $ pType parser) (Just defaultValue)
    }]
  , ifParser = M.lookup name >>> withPath (Key (unName name) :) . \case
      Just value -> parseValue value
      Nothing    -> pInputParser parser $ literal defaultValue
  }
  where
    parseValue = \value -> case value of
      VVariable (Variable { vValue, vDefinition }) ->
        -- This case is tricky: if we get a nullable variable, we have to
        -- pessimistically mark the query non-reusable, regardless of its
        -- contents. Theoretically, we could be smarter: we could create a sort
        -- of “derived variable” with its own default value and pass that to the
        -- downstream parser. But that would be more complicated, so for now we
        -- don’t do that.
        -- FIXME: Clarify this explanation.
        case dInfo vDefinition of
          IFRequired _ -> parseValue value
          IFOptional _ _ -> markNotReusable *> parseValue (literal vValue)
      VNull -> pInputParser parser $ literal defaultValue
      other -> pInputParser parser other

-- | A nullable field with no default value. If the field is omitted, the
-- provided parser /will not be called/. This allows a field to distinguish an
-- omitted field from a field supplied with @null@ (which is permitted by the
-- GraphQL specification). If you want a field that defaults to @null@, combine
-- 'field' with 'nullable', instead.
fieldOptional
  :: (MonadParse m, 'Input <: k)
  => Name
  -> Maybe Description
  -> Parser k m a
  -> FieldsParser 'Input m (Maybe a)
fieldOptional name description parser = FieldsParser
  { ifDefinitions = [Definition
    { dName = name
    , dDescription = description
    , dInfo = IFOptional (discardNullability $ pType parser) Nothing
    }]
  , ifParser = M.lookup name >>> withPath (Key (unName name) :) . traverse (pInputParser parser)
  }

selectionSet
  :: MonadParse m
  => Name
  -> Maybe Description
  -> FieldsParser 'Output m a
  -> Parser 'Output m a
selectionSet name description parser = Parser
  { pType = NonNullable $ TNamed Definition
    { dName = name
    , dDescription = description
    , dInfo = TIObject (ifDefinitions parser)
    }
  , pParser = \input -> do
      let fields = input & mapMaybe \case
            -- FIXME: handle fragments
            SelectionField inputField -> Just inputField
            _ -> Nothing
      -- check for extraneous fields here, since the FieldsParser just
      -- handles parsing the fields it cares about
      for_ fields \Field{ _fName = fieldName } -> do
        unless (fieldName `S.member` fieldNames) $
          parseError $ name <<> " has no field named " <>> fieldName
      ifParser parser $! M.fromListOn _fName fields
  }
  where
    fieldNames = S.fromList (dName <$> ifDefinitions parser)

type family SelectionResult k a b where
  SelectionResult 'Both   a _ = (Name, a)
  SelectionResult 'Output a b = (Name, a, b)

selection
  :: forall k m a b
   . (MonadParse m, 'Output <: k)
  => Name
  -> Maybe Description
  -> FieldsParser 'Input m a
  -> Parser k m b
  -> FieldsParser 'Output m (Maybe (SelectionResult k a b))
selection name description argumentsParser bodyParser = FieldsParser
  { ifDefinitions = [Definition
    { dName = name
    , dDescription = description
    , dInfo = FieldInfo (ifDefinitions argumentsParser) (pType bodyParser)
    }]
  , ifParser = M.lookup name >>> withPath (Key (unName name) :) . traverse \selectionField -> do
      let Field{ _fName = fieldName, _fArguments = arguments } = selectionField
          alias = _fAlias selectionField & fromMaybe fieldName

      for_ (M.keys arguments) \argumentName -> do
        unless (argumentName `S.member` argumentNames) $
          parseError $ name <<> " has no argument named " <>> argumentName
      parsedArguments <- ifParser argumentsParser arguments

      -- see Note [The delicate balance of GraphQL kinds]
      case subKind @'Output @k of
        KRefl -> do
          parsedBody <- pParser bodyParser $ _fSelectionSet selectionField
          pure (alias, parsedArguments, parsedBody)
        KBoth -> pure (alias, parsedArguments)
  }
  where
    argumentNames = S.fromList (dName <$> ifDefinitions argumentsParser)


typeMismatch :: MonadParse m => Name -> Text -> Value Variable -> m a
typeMismatch name expected given = parseError $
  "expected " <> expected <> " for type " <> name <<> ", but found " <> describeValue given

describeValue :: Value Variable -> Text
describeValue = describeValueWith (describeValueWith absurd . vValue)

describeValueWith :: (var -> Text) -> Value var -> Text
describeValueWith describeVariable = \case
  VVariable var -> describeVariable var
  VInt _ -> "an integer"
  VFloat _ -> "a float"
  VString _ -> "a string"
  VBoolean _ -> "a boolean"
  VNull -> "null"
  VEnum _ -> "an enum value"
  VList _ -> "a list"
  VObject _ -> "an object"

-- -------------------------------------------------------------------------------------------------

data Opaque a = Opaque
  { opVariable :: Maybe (Definition (FieldInfo 'Input))
  -- ^ The variable this value came from, if any.
  , opValue :: a
  } -- Note: we intentionally don’t derive any instances here, since that would
    -- defeat the opaqueness!

openOpaque :: MonadParse m => Opaque a -> m a
openOpaque (Opaque Nothing  value) = pure value
openOpaque (Opaque (Just _) value) = markNotReusable $> value

data UnpreparedValue
  -- | A SQL value that can be parameterized over.
  = UVParameter PGColumnValue
                (Maybe (Definition (FieldInfo 'Input)))
                -- ^ The GraphQL variable this value came from, if any.
  -- | A literal SQL expression that /cannot/ be parameterized over.
  | UVLiteral SQLExp
  -- | The entire session variables JSON object.
  | UVSession
  -- | A single session variable.
  | UVSessionVar (PGType PGScalarType) SessVar

data PGColumnValue = PGColumnValue
  { pcvType :: PGColumnType
  , pcvValue :: WithScalarType PGScalarValue
  }

mkParameter :: Opaque PGColumnValue -> UnpreparedValue
mkParameter (Opaque variable value) = UVParameter value variable

-- -------------------------------------------------------------------------------------------------

column
  :: (MonadError Text m, MonadParse n)
  => PGColumnType
  -> m (Parser 'Both n (Opaque PGColumnValue))
column columnType = opaque . fmap (PGColumnValue columnType) <$> case columnType of
  PGColumnScalar scalarType -> fmap (WithScalarType scalarType) <$> case scalarType of
    PGInteger -> pure (PGValInteger <$> int)
    PGBoolean -> pure (PGValBoolean <$> boolean)
    PGFloat   -> pure (PGValDouble <$> float)
    PGText    -> pure (PGValText <$> string)
    PGVarchar -> pure (PGValVarchar <$> string)
    _         -> do
      name <- mkScalarTypeName scalarType
      pure (PGValUnknown <$> scalar name Nothing SRString) -- FIXME: is SRString right?
  PGColumnEnumReference (EnumReference tableName enumValues) ->
    case nonEmpty (M.toList enumValues) of
      Just enumValuesList -> do
        name <- mkEnumTypeName tableName
        pure (WithScalarType PGText <$> enum name Nothing (mkEnumValue <$> enumValuesList))
      Nothing -> error "empty enum values" -- FIXME
  where
    -- Sadly, this combinator is not sound in general, so we can’t export it
    -- for general-purpose use. If we did, someone could write this:
    --
    --   mkParameter <$> opaque do
    --     n <- int
    --     pure (mkIntColumnValue (n + 1))
    --
    -- Now we’d end up with a UVParameter that has a variable in it, so we’d
    -- parameterize over it. But when we’d reuse the plan, we wouldn’t know to
    -- increment the value by 1, so we’d use the wrong value!
    --
    -- We could theoretically solve this by retaining a reference to the parser
    -- itself  and re-parsing each new value, using the saved parser, which
    -- would admittedly be neat. But it’s more complicated, and it isn’t clear
    -- that it would actually be useful, so for now we don’t support it.
    opaque :: Functor m => Parser 'Both m a -> Parser 'Both m (Opaque a)
    opaque parser = parser
      { pParser = \case
          VVariable (Variable { vDefinition, vValue }) ->
            Opaque (Just vDefinition) <$> pParser parser (literal vValue)
          value -> Opaque Nothing <$> pParser parser value
      }

    -- FIXME: unify these types, avoid unsafe conversion to Name
    mkEnumValue (RQL.EnumValue value, RQL.EnumValueInfo description) =
      ( Definition (unsafeMkName value) (Description <$> description) EnumValueInfo
      , PGValText value )

    mkScalarTypeName scalarType = mkName (toSQLTxt scalarType) `onNothing` throwError
      ("cannot use SQL type " <> scalarType <<> " in GraphQL schema because its name is not a "
      <> "valid GraphQL identifier")
    mkEnumTypeName tableName = mkName (snakeCaseQualObject tableName) `onNothing` throwError
      ("cannot use " <> tableName <<> " as an enum table because its name is not a valid "
      <> "GraphQL identifier")

-- -------------------------------------------------------------------------------------------------

-- type OpExp = OpExpG UnresolvedVal
--
-- mkBoolExpr
--   :: MonadError Text m
--   => Name
--   -> Maybe Description
--   -> FieldsParser 'Input m [a]
--   -> Parser 'Input m (GBoolExp a)
-- mkBoolExpr name description fields =
--   fix \recur -> BoolAnd <$> object name description do
--     basicFields <- map BoolFld <$> fields
--     specialFields <- catMaybes <$> sequenceA
--       [ fieldOptional "_or" Nothing (BoolOr <$> list recur)
--       , fieldOptional "_and" Nothing (BoolAnd <$> list recur)
--       , fieldOptional "_not" Nothing (BoolNot <$> recur)
--       ]
--     pure (basicFields ++ specialFields)
--
-- comparisonExprs :: MonadError Text m => PGColumnType -> Parser 'Input m [OpExp]
-- comparisonExprs columnType = object name Nothing $ catMaybes <$> sequenceA
--   [ fieldOptional "_cast" Nothing (ACast <$> castExpr columnType)
--   , fieldOptional "_eq" Nothing (AEQ True . UVPG . AnnPGVal _ _ <$> column columnType)
--   -- etc.
--   ]
--   where
--     name = unNamedType (mkColumnType columnType & addTypeSuffix "_comparison_exp")
--
-- castExpr :: forall m. MonadError Text m => PGColumnType -> Parser 'Input m (CastExp UnresolvedVal)
-- castExpr sourceType = object name Nothing (M.fromList . catMaybes <$> traverse mkField targetTypes)
--   where
--     name = unNamedType (mkColumnType sourceType & addTypeSuffix "_cast_exp")
--     targetTypes = case sourceType of
--       PGColumnScalar PGGeometry -> [PGGeography]
--       PGColumnScalar PGGeography -> [PGGeometry]
--       _ -> []
--
--     mkField :: PGScalarType -> FieldsParser 'Input m (Maybe (PGScalarType, [OpExp]))
--     mkField targetType = fieldOptional fieldName Nothing $
--       (targetType,) <$> comparisonExprs (PGColumnScalar targetType)
--       where
--         fieldName = unNamedType $ mkColumnType $ PGColumnScalar targetType
