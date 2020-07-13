{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StrictData          #-}

-- | Defines the 'Parser' type and its primitive combinators.
module Hasura.GraphQL.Parser.Internal.Parser where

import           Hasura.Prelude

import qualified Data.Aeson                    as A
import qualified Data.HashMap.Strict.Extended  as M
import qualified Data.HashMap.Strict.InsOrd    as OMap
import qualified Data.HashSet                  as S

import           Control.Lens.Extended         hiding (enum, index)
import           Data.Int                      (Int32)
import           Data.Parser.JSONPath
import           Data.Type.Equality
import           Language.GraphQL.Draft.Syntax hiding (Definition)

import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Collect
import           Hasura.GraphQL.Parser.Schema
import           Hasura.Server.Utils           (englishList)
import           Hasura.SQL.Types

-- -----------------------------------------------------------------------------
-- type definitions

-- | A 'Parser' that corresponds to a type in the GraphQL schema. A 'Parser' is
-- really two things at once:
--
--   1. As its name implies, a 'Parser' can be used to parse GraphQL queries
--      (via 'runParser').
--
--   2. Less obviously, a 'Parser' represents a slice of the GraphQL schema,
--      since every 'Parser' corresponds to a particular GraphQL type, and
--      information about that type can be recovered (via 'parserType').
--
-- A natural way to view this is that 'Parser's support a sort of dynamic
-- reflection: in addition to running a 'Parser' on an input query, you can ask
-- it to tell you about what type of input it expects. Importantly, you can do
-- this even if you don’t have a query to parse; this is necessary to implement
-- GraphQL introspection, which provides precisely this sort of reflection on
-- types.
--
-- Another way of viewing a 'Parser' is a little more quantum: just as light
-- “sometimes behaves like a particle and sometimes behaves like a wave,” a
-- 'Parser' “sometimes behaves like a query parser and sometimes behaves like a
-- type.” In this way, you can think of a function that produces a 'Parser' as
-- simultaneously both a function that constructs a GraphQL schema and a
-- function that parses a GraphQL query. 'Parser' constructors therefore
-- interleave two concerns: information about a type definition (like the type’s
-- name and description) and information about how to parse a query on that type.
--
-- Notably, these two concerns happen at totally different phases in the
-- program: GraphQL schema construction happens when @graphql-engine@ first
-- starts up, before it receives any GraphQL queries at all. But query parsing
-- obviously can’t happen until there is actually a query to parse. For that
-- reason, it’s useful to take care to distinguish which effects are happening
-- at which phase during 'Parser' construction, since otherwise you may get
-- mixed up!
--
-- For some more information about how to interpret the meaning of a 'Parser',
-- see Note [The meaning of Parser 'Output].
data Parser k m a = Parser
  { pType   :: ~(Type k)
  -- ^ Lazy for knot-tying reasons; see Note [Tying the knot] in
  -- Hasura.GraphQL.Parser.Class.
  , pParser :: ParserInput k -> m a
  } deriving (Functor)

parserType :: Parser k m a -> Type k
parserType = pType

runParser :: Parser k m a -> ParserInput k -> m a
runParser = pParser

instance HasName (Parser k m a) where
  getName = getName . pType

instance HasDefinition (Parser k m a) (TypeInfo k) where
  definitionLens f parser = definitionLens f (pType parser) <&> \pType -> parser { pType }

type family ParserInput k where
  -- see Note [The 'Both kind] in Hasura.GraphQL.Parser.Schema
  ParserInput 'Both = Value Variable
  ParserInput 'Input = Value Variable
  -- see Note [The meaning of Parser 'Output]
  ParserInput 'Output = SelectionSet NoFragments Variable

{- Note [The meaning of Parser 'Output]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The ParserInput type family determines what a Parser accepts as input during
query parsing, which varies based on its Kind. A `Parser 'Input`,
unsurprisingly, parses GraphQL input values, much in the same way aeson
`Parser`s parse JSON values.

Therefore, one might naturally conclude that `Parser 'Output` ought to parse
GraphQL output values. But it doesn’t---a Parser is used to parse GraphQL
*queries*, and output values don’t show up in queries anywhere! Rather, the
output values are the results of executing the query, not something the user
sends us, so we don’t have to parse those at all.

What output types really correspond to in GraphQL queries is selection sets. For
example, if we have the GraphQL types

    type User {
      posts(filters: PostFilters): [Post]
    }

    input PostFilters {
      newer_than: Date
    }

    type Post {
      id: Int
      title: String
      body: String
    }

then we might receive a query that looks like this:

    query list_user_posts($user_id: Int, $date: Date) {
      user_by_id(id: $user_id) {
        posts(filters: {newer_than: $date}) {
          id
          title
        }
      }
    }

We have Parsers to represent each of these types: a `Parser 'Input` for
PostFilters, and two `Parser 'Output`s for User and Post. When we parse the
query, we pass the `{newer_than: $date}` input value to the PostFilters parser,
as expected. But what do we pass to the User parser? The answer is this
selection set:

    {
      posts(filters: {newer_than: $date}) {
        id
        title
      }
    }

Likewise, the Post parser eventually receives the inner selection set:

    {
      id
      title
    }

These Parsers handle interpreting the fields of the selection sets. This is why
`ParserInput 'Output` is SelectionSet---the GraphQL *type* associated with the
Parser is an output type, but the part of the *query* that corresponds to that
output type isn’t an output value but a selection set. -}

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

-- | Parses some collection of input fields. Build an 'InputFieldsParser' using
-- 'field', 'fieldWithDefault', or 'fieldOptional', combine several together
-- with the 'Applicative' instance, and finish it off using 'object' to turn it
-- into a 'Parser'.
data InputFieldsParser m a = InputFieldsParser
  -- Note: this is isomorphic to
  --     Compose ((,) [Definition (FieldInfo k)])
  --             (ReaderT (HashMap Name (FieldInput k)) m) a
  -- but working with that type sucks.
  { ifDefinitions :: [Definition InputFieldInfo]
  , ifParser      :: HashMap Name (Value Variable) -> m a
  } deriving (Functor)

infixl 1 `bindFields`
bindFields :: Monad m => InputFieldsParser m a -> (a -> m b) -> InputFieldsParser m b
bindFields p f = p { ifParser = ifParser p >=> f }

instance Applicative m => Applicative (InputFieldsParser m) where
  pure v = InputFieldsParser [] (const $ pure v)
  a <*> b = InputFieldsParser
    (ifDefinitions a <> ifDefinitions b)
    (liftA2 (<*>) (ifParser a) (ifParser b))

-- | A parser for a single field in a selection set. Build a 'FieldParser'
-- with 'selection' or 'subselection', and combine them together with
-- 'selectionSet' to obtain a 'Parser'.
data FieldParser m a = FieldParser
  { fDefinition :: Definition FieldInfo
  , fParser     :: Field NoFragments Variable -> m a
  } deriving (Functor)

infixl 1 `bindField`
bindField :: Monad m => FieldParser m a -> (a -> m b) -> FieldParser m b
bindField p f = p { fParser = fParser p >=> f }

-- | A single parsed field in a selection set.
data ParsedSelection a
  -- | An ordinary field.
  = SelectField a
  -- | The magical @__typename@ field, implicitly available on all objects
  -- <as part of GraphQL introspection http://spec.graphql.org/June2018/#sec-Type-Name-Introspection>.
  | SelectTypename Name
  deriving (Functor)

handleTypename :: (Name -> a) -> ParsedSelection a -> a
handleTypename _ (SelectField value)   = value
handleTypename f (SelectTypename name) = f name

-- -----------------------------------------------------------------------------
-- combinators

data ScalarRepresentation a where
  SRBoolean :: ScalarRepresentation Bool
  SRInt :: ScalarRepresentation Int32
  SRFloat :: ScalarRepresentation Double
  SRString :: ScalarRepresentation Text
  -- TODO should this have an "others" case?
  SRAny :: ScalarRepresentation A.Value

scalar
  :: MonadParse m
  => Name
  -> Maybe Description
  -> ScalarRepresentation a
  -> Parser 'Both m a
scalar name description representation = Parser
  { pType = schemaType
  , pParser = peelVariable (Just $ toGraphQLType schemaType) >=> \v -> case representation of
      SRBoolean -> case v of
        VBoolean a -> pure a
        _          -> typeMismatch name "a boolean" v
      SRInt -> case v of
        VInt a -> pure a
        _      -> typeMismatch name "an integer" v
      SRFloat -> case v of
        VFloat a -> pure a
        VInt   a -> pure $ fromIntegral a
        _        -> typeMismatch name "a float" v
      SRString -> case v of
        VString _ a -> pure a
        _           -> typeMismatch name "a string" v
      SRAny -> case v of
        VNull               -> pure A.Null
        VInt i              -> pure $ A.toJSON i
        VFloat f            -> pure $ A.toJSON f
        VString _ t         -> pure $ A.toJSON t
        VBoolean b          -> pure $ A.toJSON b
        _                   -> typeMismatch name "a scalar" v
  }
  where
    schemaType = NonNullable $ TNamed $ mkDefinition name description TIScalar

boolean :: MonadParse m => Parser 'Both m Bool
boolean = scalar $$(litName "Boolean") Nothing SRBoolean

int :: MonadParse m => Parser 'Both m Int32
int = scalar $$(litName "Int") Nothing SRInt

float :: MonadParse m => Parser 'Both m Double
float = scalar $$(litName "Float") Nothing SRFloat

string :: MonadParse m => Parser 'Both m Text
string = scalar $$(litName "String") Nothing SRString

identifier :: MonadParse m => Parser 'Both m Text
identifier = scalar $$(litName "ID") Nothing SRString

{-
jsonScalar
  :: MonadParse m
  => Name -> Maybe Description -> Parser 'Both m A.Value
jsonScalar name description = scalar name description SRAny
-}

namedJSON :: MonadParse m => Name -> Parser 'Both m A.Value
namedJSON name = Parser
  { pType = NonNullable $ TNamed $ mkDefinition name Nothing TIScalar
  , pParser = graphQLToJSON
  }

json, jsonb :: MonadParse m => Parser 'Both m A.Value
json  = namedJSON $$(litName "json")
jsonb = namedJSON $$(litName "jsonb")

-- | Explicitly define any desired scalar type.  This is unsafe because it does
-- not mark queries as unreusable when they should be.
unsafeRawScalar
  :: MonadParse n
  => Name
  -> Maybe Description
  -> Parser 'Both n (Value Variable)
unsafeRawScalar name description = Parser
  { pType = NonNullable $ TNamed $ mkDefinition name description TIScalar
  , pParser = pure
  }

enum
  :: MonadParse m
  => Name
  -> Maybe Description
  -> NonEmpty (Definition EnumValueInfo, a)
  -> Parser 'Both m a
enum name description values = Parser
  { pType = schemaType
  , pParser = peelVariable (Just $ toGraphQLType schemaType) >=> \value -> case value of
      VString origin stringValue -> case (origin, mkName stringValue) of
        (ExternalValue, Just enumValue) -> validate enumValue
        _ -> typeMismatch name "an enum value" value
      VEnum (EnumValue enumValue) -> validate enumValue
      _ -> typeMismatch name "an enum value" value
  }
  where
    schemaType = NonNullable $ TNamed $ mkDefinition name description $ TIEnum (fst <$> values)
    valuesMap = M.fromList $ over (traverse._1) dName $ toList values
    validate value = case M.lookup value valuesMap of
      Just result -> pure result
      Nothing -> parseError $ "expected one of the values "
        <> englishList "or" (dquoteTxt . dName . fst <$> values) <> " for type "
        <> name <<> ", but found " <>> value

nullable :: forall k m a. (MonadParse m, 'Input <: k) => Parser k m a -> Parser k m (Maybe a)
nullable parser = gcastWith (inputParserInput @k) Parser
  { pType = schemaType
  , pParser = peelVariable (Just $ toGraphQLType schemaType) >=> \case
      VNull -> pure Nothing
      other -> Just <$> pParser parser other
  }
  where
    schemaType = case pType parser of
      NonNullable t -> Nullable t
      Nullable t    -> Nullable t

-- | Decorate a type as NON_NULL
nonNullableType :: forall k . Type k -> Type k
nonNullableType = \case
  NonNullable t -> NonNullable t
  Nullable t    -> NonNullable t

-- | Decorate a type as NULL.  Note that regardless of what we think, a nullable
-- value read from the user should be represented by a 'Maybe' Haskell type (or
-- another kind of partial data).
nullableType :: forall k . Type k -> Type k
nullableType = \case
  NonNullable t -> Nullable t
  Nullable t    -> Nullable t

-- | Decorate a schema field as NON_NULL
nonNullableField :: forall m a . FieldParser m a -> FieldParser m a
nonNullableField (FieldParser (Definition n u d (FieldInfo as t)) p) =
  (FieldParser (Definition n u d (FieldInfo as (nonNullableType t))) p)

-- | Decorate a schema field as NULL
nullableField :: forall m a . FieldParser m a -> FieldParser m a
nullableField (FieldParser (Definition n u d (FieldInfo as t)) p) =
  (FieldParser (Definition n u d (FieldInfo as (nullableType t))) p)
{-
field = field
  { fDefinition = (fDefinition field)
    { dInfo = (dInfo (fDefinition field))
      { fType = nonNullableType (fType (dInfo (fDefinition field)))
      }
    }
  }
-}
-- | Decorate a schema output type as NON_NULL
nonNullableParser :: forall m a . Parser 'Output m a -> Parser 'Output m a
nonNullableParser parser = parser { pType = nonNullableType (pType parser) }

list :: forall k m a. (MonadParse m, 'Input <: k) => Parser k m a -> Parser k m [a]
list parser = gcastWith (inputParserInput @k) Parser
  { pType = schemaType
  , pParser = peelVariable (Just $ toGraphQLType schemaType) >=> \case
      VList values -> for (zip [0..] values) \(index, value) ->
        withPath (Index index :) $ pParser parser value
      -- List Input Coercion
      --
      -- According to section 3.11 of the GraphQL spec: iff the value
      -- passed as an input to a list type is not a list and not the
      -- null value, then the result of input coercion is a list of
      -- size one, where the single item value is the result of input
      -- coercion for the list’s item type on the provided value.
      --
      -- We need to explicitly test for VNull here, otherwise we could
      -- be returning `[null]` if the parser accepts a null value,
      -- which would contradict the spec.
      VNull -> parseError "expected a list, but found null"
      other -> fmap pure $ withPath (Index 0 :) $ pParser parser other
  }
  where
    schemaType = NonNullable $ TList $ pType parser

object
  :: MonadParse m
  => Name
  -> Maybe Description
  -> InputFieldsParser m a
  -> Parser 'Input m a
object name description parser = Parser
  { pType = schemaType
  , pParser = peelVariable (Just $ toGraphQLType schemaType) >=> \case
      VObject fields -> do
        -- check for extraneous fields here, since the InputFieldsParser just
        -- handles parsing the fields it cares about
        for_ (M.keys fields) \fieldName -> do
          unless (fieldName `S.member` fieldNames) $
            parseError $ name <<> " has no field named " <>> fieldName
        ifParser parser fields
      other -> typeMismatch name "an object" other
  }
  where
    schemaType = NonNullable $ TNamed $ mkDefinition name description $
                 TIInputObject (ifDefinitions parser)
    fieldNames = S.fromList (dName <$> ifDefinitions parser)

field
  :: (MonadParse m, 'Input <: k)
  => Name
  -> Maybe Description
  -> Parser k m a
  -> InputFieldsParser m a
field name description parser = case pType parser of
  NonNullable _ -> InputFieldsParser
    { ifDefinitions = [mkDefinition name description case pType parser of
        NonNullable typ -> IFRequired typ
        Nullable typ    -> IFOptional typ (Just VNull)]
    , ifParser = \ values -> withPath (++[Key (unName name)]) do
        value <- onNothing (M.lookup name values) $
          parseError ("missing required field " <>> name)
        pInputParser parser value
    }
  -- nullable fields just have an implicit default value of `null`
  Nullable _ -> fieldWithDefault name description VNull parser

fieldWithDefault
  :: (MonadParse m, 'Input <: k)
  => Name
  -> Maybe Description
  -> Value Void -- ^ default value
  -> Parser k m a
  -> InputFieldsParser m a
fieldWithDefault name description defaultValue parser = InputFieldsParser
  { ifDefinitions = [mkDefinition name description $
      IFOptional (discardNullability $ pType parser) (Just defaultValue)]
  , ifParser = M.lookup name >>> withPath (++[Key (unName name)]) . \case
      Just value -> parseValue (toGraphQLType $ pType parser) value
      Nothing    -> pInputParser parser $ literal defaultValue
      -- FIXME: we use the literal without checking that its value can
      -- in theory be assigned to the declared type
  }
  where
    parseValue expectedType value = case value of
      VVariable (var@Variable { vInfo, vValue }) -> do
        typeCheck expectedType var
        -- This case is tricky: if we get a nullable variable, we have to
        -- pessimistically mark the query non-reusable, regardless of its
        -- contents. Why? Well, suppose we have a type like
        --
        --     type Foo {
        --       bar(arg: Int = 42): String
        --     }
        --
        -- and suppose we receive the following query:
        --
        --     query blah($var: Int) {
        --       foo {
        --         bar(arg: $var)
        --       }
        --     }
        --
        -- Suppose no value is provided for $var, so it defaults to null. When
        -- we parse the arg field, we see it has a default value, so we
        -- substitute 42 for null and carry on. But now we’ve discarded the
        -- information that this value came from a variable at all, so if we
        -- cache the query plan, changes to the variable will be ignored, since
        -- we’ll always use 42!
        --
        -- Note that the problem doesn’t go away even if $var has a non-null
        -- value. In that case, we’d simply have flipped the problem around: now
        -- our cached query plan will do the wrong thing if $var *is* null,
        -- since we won’t know to substitute 42.
        --
        -- Theoretically, we could be smarter here: we could record a sort of
        -- “derived variable reference” that includes a new default value. But
        -- that would be more complicated, so for now we don’t do that.
        case vInfo of
          VIRequired _   -> pInputParser parser value
          VIOptional _ _ -> markNotReusable *> parseValue expectedType (literal vValue)
      VNull -> pInputParser parser $ literal defaultValue
      _     -> pInputParser parser value

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
  -> InputFieldsParser m (Maybe a)
fieldOptional name description parser = InputFieldsParser
  { ifDefinitions = [mkDefinition name description $
      IFOptional (discardNullability $ pType parser) Nothing]
  , ifParser = M.lookup name >>> withPath (++[Key (unName name)]) . traverse (pInputParser parser)
  }

-- | A variant of 'selectionSetObject' which doesn't implement any interfaces
selectionSet
  :: MonadParse m
  => Name
  -> Maybe Description
  -> [FieldParser m a]
  -> Parser 'Output m (OMap.InsOrdHashMap Name (ParsedSelection a))
selectionSet name desc fields = selectionSetObject name desc fields []

-- Should this rather take a non-empty `FieldParser` list?
-- See also Note [Selectability of tables].
selectionSetObject
  :: MonadParse m
  => Name
  -> Maybe Description
  -> [FieldParser m a]
  -> [Parser 'Output m b] -- ^ Interfaces implemented by this object
  -> Parser 'Output m (OMap.InsOrdHashMap Name (ParsedSelection a))
selectionSetObject name description parsers implementsInterfaces = Parser
  { pType = Nullable $ TNamed $ mkDefinition name description $
      TIObject $ ObjectInfo (map fDefinition parsers) interfaces
  , pParser = \input -> withPath (++[Key "selectionSet"]) do
      -- Not all fields have a selection set, but if they have one, it
      -- must contain at least one field. The GraphQL parser returns a
      -- list to represent this: an empty list indicates there was no
      -- selection set, as an empty set is rejected outright.
      -- Arguably, this would be better represented by a `Maybe
      -- (NonEmpty a)`.
      -- The parser can't know whether a given field needs a selection
      -- set or not; but if we're in this function, it means that yes:
      -- this field needs a selection set, and if none was provided,
      -- we must fail.
      when (null input) $
        parseError $ "missing selection set for " <>> name

      -- TODO This probably accepts invalid queries, namely queries that use
      -- type names that do not exist.
      fields <- collectFields (name:parsedInterfaceNames) input
      for fields \selectionField@Field{ _fName, _fAlias } -> if
        | _fName == $$(litName "__typename") ->
            pure $ SelectTypename name
        | Just parser <- M.lookup _fName parserMap ->
            withPath (++[Key (unName _fName)]) $
              SelectField <$> parser selectionField
        | otherwise ->
            withPath (++[Key (unName _fName)]) $
            parseError $ "field " <> _fName <<> " not found in type: " <> squote name
  }
  where
    parserMap = parsers
      & map (\FieldParser{ fDefinition, fParser } -> (getName fDefinition, fParser))
      & M.fromList
    interfaces = catMaybes $ fmap (getInterfaceInfo . pType) implementsInterfaces
    collectNames :: Definition InterfaceInfo -> [Name]
    collectNames defIface = (getName defIface :) $ concat $ map collectNames $
      case dInfo defIface of
        InterfaceInfo _fields subInterfaces _objs -> subInterfaces
    parsedInterfaceNames = concat $ fmap collectNames interfaces

selectionSetInterface
  :: (MonadParse n, Traversable tObjs, Traversable tIfaces)
  => Name
  -> Maybe Description
  -> [FieldParser n a]
  -- ^ Fields defined in this interface
  -> tObjs (Parser 'Output n b)
  -- ^ Object parsers.  The objects should implement this interface.
  -> tIfaces (Parser 'Output n c)
  -- ^ The interfaces that this interface implements (for typing information only).
  -> Parser 'Output n
       ( -- For each object or interface, give its parsing
         tObjs b
       )
selectionSetInterface name description fields objectImplementations implementsInterfaces = Parser
  { pType = Nullable $ TNamed $ mkDefinition name description $
      TIInterface $ InterfaceInfo (map fDefinition fields) interfaces objects
  , pParser = \input -> for objectImplementations (($ input) . pParser)
  -- TODO: This is somewhat suboptimal, since it parses a query against any
  -- possible object implementing this.  But in our intended use case (Relay),
  -- based on a field argument, we can decide which object we are about to
  -- retrieve.  So this implementation could save some work by only running that
  -- parser.
  }
  where
    objects = catMaybes $ toList $ fmap (getObjectInfo . pType) objectImplementations
    interfaces = catMaybes $ toList $ fmap (getInterfaceInfo . pType) implementsInterfaces

selectionSetUnion
  :: (MonadParse n, Traversable tObjs)
  => Name
  -> Maybe Description
  -> tObjs (Parser 'Output n b)
  -- ^ Possible objects
  -> Parser 'Output n
       ( -- For each object or interface, give its parsing
         tObjs b
       )
selectionSetUnion name description objectImplementations = Parser
  { pType = Nullable $ TNamed $ mkDefinition name description $
      TIUnion $ UnionInfo objects
  , pParser = \input -> for objectImplementations (($ input) . pParser)
  }
  where
    objects = catMaybes $ toList $ fmap (getObjectInfo . pType) objectImplementations

-- | An "escape hatch" that doesn't validate anything and just gives the
-- requested selection set.  This is unsafe because it does not check the
-- selection set for validity.
unsafeRawParser
  :: forall m
   . MonadParse m
  => Type 'Output
  -> Parser 'Output m (SelectionSet NoFragments Variable)
unsafeRawParser tp = Parser
  { pType = tp
  , pParser = pure
  }

unsafeRawField
  :: forall m
   . MonadParse m
  => Definition FieldInfo
  -> FieldParser m (Field NoFragments Variable)
unsafeRawField def = FieldParser
  { fDefinition = def
  , fParser = pure
  }

-- | Builds a 'FieldParser' for a field that does not take a subselection set,
-- i.e. a field that returns a scalar or enum. The field’s type is taken from
-- the provided 'Parser', but the 'Parser' is not otherwise used.
--
-- See also Note [The delicate balance of GraphQL kinds] in "Hasura.GraphQL.Parser.Schema".
selection
  :: forall m a b
   . MonadParse m
  => Name
  -> Maybe Description
  -> InputFieldsParser m a -- ^ parser for the input arguments
  -> Parser 'Both m b -- ^ type of the result
  -> FieldParser m a
selection name description argumentsParser resultParser = FieldParser
  { fDefinition = mkDefinition name description $
      FieldInfo (ifDefinitions argumentsParser) (pType resultParser)
  , fParser = \Field{ _fArguments, _fSelectionSet } -> do
      unless (null _fSelectionSet) $
        parseError "unexpected subselection set for non-object field"
      -- check for extraneous arguments here, since the InputFieldsParser just
      -- handles parsing the fields it cares about
      for_ (M.keys _fArguments) \argumentName -> do
        unless (argumentName `S.member` argumentNames) $
          parseError $ name <<> " has no argument named " <>> argumentName
      withPath (++[Key "args"]) $ ifParser argumentsParser _fArguments
  }
  where
    argumentNames = S.fromList (dName <$> ifDefinitions argumentsParser)

-- | Builds a 'FieldParser' for a field that takes a subselection set, i.e. a
-- field that returns an object.
--
-- See also Note [The delicate balance of GraphQL kinds] in "Hasura.GraphQL.Parser.Schema".
subselection
  :: forall m a b
   . MonadParse m
  => Name
  -> Maybe Description
  -> InputFieldsParser m a -- ^ parser for the input arguments
  -> Parser 'Output m b -- ^ parser for the subselection set
  -> FieldParser m (a, b)
subselection name description argumentsParser bodyParser = FieldParser
  { fDefinition = mkDefinition name description $
      FieldInfo (ifDefinitions argumentsParser) (pType bodyParser)
  , fParser = \Field{ _fArguments, _fSelectionSet } -> do
      -- check for extraneous arguments here, since the InputFieldsParser just
      -- handles parsing the fields it cares about
      for_ (M.keys _fArguments) \argumentName -> do
        unless (argumentName `S.member` argumentNames) $
          parseError $ name <<> " has no argument named " <>> argumentName
      (,) <$> withPath (++[Key "args"]) (ifParser argumentsParser _fArguments)
          <*> pParser bodyParser _fSelectionSet
  }
  where
    argumentNames = S.fromList (dName <$> ifDefinitions argumentsParser)

-- | A shorthand for a 'selection' that takes no arguments.
selection_
  :: MonadParse m
  => Name
  -> Maybe Description
  -> Parser 'Both m a -- ^ type of the result
  -> FieldParser m ()
selection_ name description = selection name description (pure ())

-- | A shorthand for a 'subselection' that takes no arguments.
subselection_
  :: MonadParse m
  => Name
  -> Maybe Description
  -> Parser 'Output m a -- ^ parser for the subselection set
  -> FieldParser m a
subselection_ name description bodyParser =
  snd <$> subselection name description (pure ()) bodyParser


-- -----------------------------------------------------------------------------
-- helpers

graphQLToJSON :: MonadParse m => Value Variable -> m A.Value
graphQLToJSON = peelVariable Nothing >=> \case
  VNull               -> pure A.Null
  VInt i              -> pure $ A.toJSON i
  VFloat f            -> pure $ A.toJSON f
  VString _ t         -> pure $ A.toJSON t
  VBoolean b          -> pure $ A.toJSON b
  VEnum (EnumValue n) -> pure $ A.toJSON n
  VList values        -> A.toJSON <$> traverse graphQLToJSON values
  VObject objects     -> A.toJSON <$> traverse graphQLToJSON objects
  -- this should not be possible, as we peel any variable first
  VVariable _         -> error "FIMXE: this should be a 500 with a descriptive message"

peelVariable :: MonadParse m => Maybe GType -> Value Variable -> m (Value Variable)
peelVariable expected (VVariable (var@Variable { vValue })) = do
  onJust expected \locationType -> typeCheck locationType var
  pure $ literal vValue
peelVariable _ value = pure value

typeCheck :: MonadParse m => GType -> Variable -> m ()
typeCheck expectedType (Variable { vInfo, vType }) =
  unless (True {-expectedType `accepts` vType -}) $ parseError
    $ "variable " <> unName (getName vInfo) <> " of type "
    <> showGT vType <> " is used in position expecting "
    <> showGT expectedType

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
  VString _ _ -> "a string"
  VBoolean _ -> "a boolean"
  VNull -> "null"
  VEnum _ -> "an enum value"
  VList _ -> "a list"
  VObject _ -> "an object"

-- | Checks whether the type of a variable is compatible with the type
--   at the location at which it is used.
accepts :: GType -> GType -> Bool
expected `accepts` actual = case (expected, actual) of
  (TypeNamed expectedNullability expectedType, TypeNamed actualNullability actualType)
    -> checkNullability expectedNullability actualNullability && (expectedType == actualType)
  (TypeList expectedNullability expectedType, TypeList actualNullability actualType)
    -> checkNullability expectedNullability actualNullability && (expectedType `accepts` actualType)
  _ -> False
  where
    checkNullability (Nullability expectedNullability) (Nullability actualNullability) =
      expectedNullability || not actualNullability
