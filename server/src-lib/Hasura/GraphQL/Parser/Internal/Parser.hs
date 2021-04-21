{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE StrictData #-}

-- | Defines the 'Parser' type and its primitive combinators.
module Hasura.GraphQL.Parser.Internal.Parser
  ( module Hasura.GraphQL.Parser.Internal.Parser
  , Parser(..)
  , parserType
  , runParser
  , ParserInput
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                           as A
import qualified Data.HashMap.Strict.Extended         as M
import qualified Data.HashMap.Strict.InsOrd           as OMap
import qualified Data.HashSet                         as S
import qualified Data.List.Extended                   as LE
import qualified Data.UUID                            as UUID

import           Control.Lens.Extended                hiding (enum, index)
import           Data.Int                             (Int32, Int64)
import           Data.Parser.JSONPath
import           Data.Scientific                      (toBoundedInteger)
import           Data.Text.Extended
import           Data.Type.Equality
import           Language.GraphQL.Draft.Syntax        hiding (Definition)

import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.GraphQL.Parser.Class.Parse
import           Hasura.GraphQL.Parser.Collect
import           Hasura.GraphQL.Parser.Internal.Types
import           Hasura.GraphQL.Parser.Schema
import           Hasura.RQL.Types.CustomTypes
import           Hasura.RQL.Types.Error
import           Hasura.Server.Utils                  (englishList)


-- | The constraint @(''Input' '<:' k)@ entails @('ParserInput' k ~ 'Value')@,
-- but GHC can’t figure that out on its own, so we have to be explicit to give
-- it a little help.
inputParserInput :: forall k. 'Input <: k => ParserInput k :~: InputValue Variable
inputParserInput = case subKind @'Input @k of { KRefl -> Refl; KBoth -> Refl }

pInputParser :: forall k m a. 'Input <: k => Parser k m a -> InputValue Variable -> m a
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
  , ifParser      :: HashMap Name (InputValue Variable) -> m a
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

instance HasDefinition (FieldParser m a) FieldInfo where
  definitionLens f parser = definitionLens f (fDefinition parser) <&> \fDefinition -> parser { fDefinition }

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
        GraphQLValue (VBoolean b) -> pure b
        JSONValue    (A.Bool   b) -> pure b
        _                         -> typeMismatch name "a boolean" v
      SRInt -> case v of
        GraphQLValue (VInt i)  -> convertWith scientificToInteger $ fromInteger i
        JSONValue (A.Number n) -> convertWith scientificToInteger n
        _                      -> typeMismatch name "a 32-bit integer" v
      SRFloat -> case v of
        GraphQLValue (VFloat f)   -> convertWith scientificToFloat f
        GraphQLValue (VInt   i)   -> convertWith scientificToFloat $ fromInteger i
        JSONValue    (A.Number n) -> convertWith scientificToFloat n
        _                         -> typeMismatch name "a float" v
      SRString -> case v of
        GraphQLValue (VString  s) -> pure s
        JSONValue    (A.String s) -> pure s
        _                         -> typeMismatch name "a string" v
  }
  where
    schemaType = NonNullable $ TNamed $ mkDefinition name description TIScalar
    convertWith f = either (parseErrorWith ParseFailed . qeError) pure . runAesonParser f

{- WIP NOTE (FIXME: make into an actual note by expanding on it a bit)

There's a delicate balance between GraphQL types and Postgres types.

The mapping is done in the 'column' parser. But we want to only have
one source of truth for parsing postgres values, which happens to be
the JSON parsing code in Backends.Postgres.SQL.Value. So here we reuse
some of that code despite not having a JSON value.

-}

boolean :: MonadParse m => Parser 'Both m Bool
boolean = scalar boolScalar Nothing SRBoolean

int :: MonadParse m => Parser 'Both m Int32
int = scalar intScalar Nothing SRInt

float :: MonadParse m => Parser 'Both m Double
float = scalar floatScalar Nothing SRFloat

string :: MonadParse m => Parser 'Both m Text
string = scalar stringScalar Nothing SRString

uuid :: MonadParse m => Parser 'Both m UUID.UUID
uuid = Parser
  { pType = schemaType
  , pParser = peelVariable (Just $ toGraphQLType schemaType) >=> \case
      GraphQLValue (VString s) -> parseUUID $ A.String s
      JSONValue    v           -> parseUUID v
      v                        -> typeMismatch name "a UUID" v
  }
  where
    name = $$(litName "uuid")
    schemaType = NonNullable $ TNamed $ mkDefinition name Nothing TIScalar
    parseUUID = either (parseErrorWith ParseFailed . qeError) pure . runAesonParser A.parseJSON

-- | As an input type, any string or integer input value should be coerced to ID as Text
-- https://spec.graphql.org/June2018/#sec-ID
identifier :: MonadParse m => Parser 'Both m Text
identifier = Parser
  { pType = schemaType
  , pParser = peelVariable (Just $ toGraphQLType schemaType) >=> \case
      GraphQLValue (VString  s) -> pure s
      GraphQLValue (VInt     i) -> pure $ tshow i
      JSONValue    (A.String s) -> pure s
      JSONValue    (A.Number n) -> parseScientific n
      v                         -> typeMismatch idName "a String or a 32-bit integer" v
  }
  where
    idName = idScalar
    schemaType = NonNullable $ TNamed $ mkDefinition idName Nothing TIScalar
    parseScientific = either (parseErrorWith ParseFailed . qeError)
      (pure . tshow @Int) . runAesonParser scientificToInteger

namedJSON :: MonadParse m => Name -> Maybe Description -> Parser 'Both m A.Value
namedJSON name description = Parser
  { pType = schemaType
  , pParser = valueToJSON $ toGraphQLType schemaType
  }
  where
    schemaType = NonNullable $ TNamed $ mkDefinition name description TIScalar

json, jsonb :: MonadParse m => Parser 'Both m A.Value
json  = namedJSON $$(litName "json") Nothing
jsonb = namedJSON $$(litName "jsonb") Nothing

-- | Explicitly define any desired scalar type.  This is unsafe because it does
-- not mark queries as unreusable when they should be.
unsafeRawScalar
  :: MonadParse n
  => Name
  -> Maybe Description
  -> Parser 'Both n (InputValue Variable)
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
  , pParser = peelVariable (Just $ toGraphQLType schemaType) >=> \case
      JSONValue (A.String stringValue)
        | Just enumValue <- mkName stringValue -> validate enumValue
      GraphQLValue (VEnum (EnumValue enumValue)) -> validate enumValue
      other -> typeMismatch name "an enum value" other
  }
  where
    schemaType = NonNullable $ TNamed $ mkDefinition name description $ TIEnum (fst <$> values)
    valuesMap = M.fromList $ over (traverse._1) dName $ toList values
    validate value = onNothing (M.lookup value valuesMap) $
      parseError $ "expected one of the values "
        <> englishList "or" (toTxt . dName . fst <$> values) <> " for type "
        <> name <<> ", but found " <>> value

nullable :: forall k m a. (MonadParse m, 'Input <: k) => Parser k m a -> Parser k m (Maybe a)
nullable parser = gcastWith (inputParserInput @k) Parser
  { pType = schemaType
  , pParser = peelVariable (Just $ toGraphQLType schemaType) >=> \case
      JSONValue    A.Null -> pure Nothing
      GraphQLValue VNull  -> pure Nothing
      value               -> Just <$> pParser parser value
  }
  where
    schemaType = nullableType $ pType parser

-- | Decorate a schema field as NON_NULL
nonNullableField :: forall m a . FieldParser m a -> FieldParser m a
nonNullableField (FieldParser (Definition n u d (FieldInfo as t)) p) =
  FieldParser (Definition n u d (FieldInfo as (nonNullableType t))) p

-- | Decorate a schema field as NULL
nullableField :: forall m a . FieldParser m a -> FieldParser m a
nullableField (FieldParser (Definition n u d (FieldInfo as t)) p) =
  FieldParser (Definition n u d (FieldInfo as (nullableType t))) p
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

-- | Make a schema output as nullable
nullableParser :: forall m a . Parser 'Output m a -> Parser 'Output m a
nullableParser parser = parser { pType = nullableType (pType parser) }

multiple :: Parser 'Output m a -> Parser 'Output m a
multiple parser = parser { pType = Nullable $ TList $ pType parser }

list :: forall k m a. (MonadParse m, 'Input <: k) => Parser k m a -> Parser k m [a]
list parser = gcastWith (inputParserInput @k) Parser
  { pType = schemaType
  , pParser = peelVariable (Just $ toGraphQLType schemaType) >=> \case
      GraphQLValue (VList values) -> for (zip [0..] values) \(index, value) ->
        withPath (++[Index index]) $ pParser parser $ GraphQLValue value
      JSONValue (A.Array values) -> for (zip [0..] $ toList values) \(index, value) ->
        withPath (++[Index index]) $ pParser parser $ JSONValue value
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
      GraphQLValue VNull  -> parseError "expected a list, but found null"
      JSONValue    A.Null -> parseError "expected a list, but found null"
      other -> fmap pure $ withPath (++[Index 0]) $ pParser parser other
  }
  where
    schemaType = NonNullable $ TList $ pType parser

-- TODO: if we had an optional "strict" mode, we could (and should!) enforce
-- that `fieldName` isn't empty, which sadly can't be done at the type level.
-- This would prevent the creation of an object with no fields, which is against
-- the spec.
object
  :: MonadParse m
  => Name
  -> Maybe Description
  -> InputFieldsParser m a
  -> Parser 'Input m a
object name description parser = Parser
  { pType = schemaType
  , pParser = peelVariable (Just $ toGraphQLType schemaType) >=> \case
      GraphQLValue (VObject fields) -> parseFields $ GraphQLValue <$> fields
      JSONValue (A.Object fields) -> do
        translatedFields <- M.fromList <$> for (M.toList fields) \(key, val) -> do
          name' <- mkName key `onNothing` parseError
            ("variable value contains object with key " <> key <<> ", which is not a legal GraphQL name")
          pure (name', JSONValue val)
        parseFields translatedFields
      other -> typeMismatch name "an object" other
  }
  where
    schemaType = NonNullable $ TNamed $ mkDefinition name description $
                 TIInputObject (InputObjectInfo (ifDefinitions parser))
    fieldNames = S.fromList (dName <$> ifDefinitions parser)
    parseFields fields = do
      -- check for extraneous fields here, since the InputFieldsParser just
      -- handles parsing the fields it cares about
      for_ (M.keys fields) \fieldName ->
        unless (fieldName `S.member` fieldNames) $ withPath (++[Key (unName fieldName)]) $
          parseError $ "field " <> dquote fieldName <> " not found in type: " <> squote name
      ifParser parser fields

{- Note [Optional fields and nullability]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GraphQL conflates optional fields and nullability. A field of a GraphQL input
object (or an argument to a selection set field, which is really the same thing)
is optional if and only if its type is nullable. It’s worth fully spelling out
the implications here: if a field (or argument) is non-nullable, it /cannot/ be
omitted. So, for example, suppose we had a table type like this:

    type article {
      comments(limit: Int!): [comment!]!
    }

Since we made `limit` non-nullable, it is /illegal/ to omit the argument. You’d
/always/ have to provide some value---and that isn’t what we want, because the
row limit should be optional. We have no choice but to make it nullable:

    type article {
      comments(limit: Int): [comment!]!
    }

But this feels questionable. Should we really accept `null` values for `limit`?
That is, should this query be legal?

    {
      articles {
        comments(limit: null) { ... }
      }
    }

A tempting answer to that question is “yes”: we can just treat a `null` value
for any optional field as precisely equivalent to leaving the field off
entirely. That is, any field with no default value really just has a default
value of `null`. Unfortunately, this approach turns out to be a really bad idea.
It’s all too easy to write something like

    mutation delete_article_by_id($article_id: Int) {
      delete_articles(where: {id: {eq: $article_id}})
    }

then accidentally misspell `article_id` in the variables payload, and now you’ve
deleted all the articles in your database. Very bad.

So we’d really like to be able to have a way to say “this field is optional, but
`null` is not a legal value,” but at first it seems like the GraphQL spec ties
our hands. Fortunately, there is a way out. The spec explicitly permits
distinguishing between the following two situations:

    comments { ... }
    comments(limit: null) { ... }

That is, the spec allows implementations to behave differently depending on
whether an argument was omitted or whether its value was `null`. This is spelled
out in a few different places in the spec, but §3.10 Input Objects
<http://spec.graphql.org/June2018/#sec-Input-Objects> is the most explicit:

> If the value `null` was provided for an input object field, and the field’s
> type is not a non‐null type, an entry in the coerced unordered map is given
> the value `null`. In other words, there is a semantic difference between the
> explicitly provided value `null` versus having not provided a value.

Note that this is only allowed for fields that don’t have any default value! If
the field were declared with an explicit `null` default value, like

    type article {
      comments(limit: Int = null): [comment!]!
    }

then it would not be legal to distinguish the two cases. Yes, this is all
terribly subtle.

Okay. So armed with that knowledge, what do we do about it? We offer three
different combinators for parsing input fields:

  1. `field` — Defines a field with no default value. The field’s nullability is
       taken directly from the nullability of the field’s value parser.
  2. `fieldOptional` — Defines a field with no default value that is always
       nullable. Returns Nothing if (and only if!) the field is omitted.
  3. `fieldWithDefault` — Defines a field with a default value.

The last of the three, `fieldWithDefault`, is actually the simplest. It
corresponds to a field with a default value, and the underlying value parser
will /always/ be called. If the field is omitted, the value parser is called
with the default value. (This makes it impossible to distinguish omitted fields
from those explicitly passed the default value, as mandated by the spec.) Use
`fieldWithDefault` for any field or argument with a non-`null` default value.

`field` is also fairly straightforward. It always calls its value parser, so if
the field is omitted, it calls it with a value of `null`. Notably, there is no
special handling for non-nullable fields, since the underlying parser will raise
an error in that case, anyway. Use `field` for required fields, and combine
`field` with `nullable` for optional fields with a default value of `null`.

`fieldOptional` is the most interesting. Unlike `field` and `fieldWithDefault`,
`fieldOptional` does not call its underlying value parser if the field is not
provided; it simply returns Nothing. If a value /is/ provided, it is passed
along without modification. This yields an interesting interaction when the
value parser does not actually accept nulls, such as a parser like this:

    fieldOptional $$(litName "limit") Nothing int

This corresponds to the `limit` field from our original example. If the field is
omitted, the `int` parser is not called, and the field parser just returns
Nothing. But if a value of `null` is explicitly provided, it is forwarded to the
`int` parser, which then rejects it with a parse error, since it does not accept
nulls. This is exactly the behavior we want.

This semantics can appear confusing. We end up with a field with a nullable type
for which `null` is not a legal value! A strange interpretation of “nullable”,
indeed. But realize that the nullability really means “optional”, and the
behavior makes more sense.

As a final point, note that similar behavior can be obtained with
`fieldWithDefault`. The following creates a boolean field that defaults to
`false` and rejects `null` values:

    fieldWithDefault $$(litName "includeDeprecated") Nothing (VBoolean False) boolean

This is a perfectly reasonable thing to do for exactly the same rationale behind
the use of `fieldOptional` above. -}

-- | Creates a parser for an input field. The field’s nullability is determined
-- by the nullability of the given value parser; see Note [Optional fields and
-- nullability] for more details.
field
  :: (MonadParse m, 'Input <: k)
  => Name
  -> Maybe Description
  -> Parser k m a
  -> InputFieldsParser m a
field name description parser = case pType parser of
  NonNullable typ -> InputFieldsParser
    { ifDefinitions = [mkDefinition name description $ IFRequired typ]
    , ifParser = \ values -> withPath (++[Key (unName name)]) do
        value <- onNothing (M.lookup name values) $
          parseError ("missing required field " <>> name)
        pInputParser parser value
    }
  -- nullable fields just have an implicit default value of `null`
  Nullable _ -> fieldWithDefault name description VNull parser

-- | Creates a parser for an input field with the given default value. The
-- resulting field will always be nullable, even if the underlying parser
-- rejects `null` values; see Note [Optional fields and nullability] for more
-- details.
fieldWithDefault
  :: (MonadParse m, 'Input <: k)
  => Name
  -> Maybe Description
  -> Value Void -- ^ default value
  -> Parser k m a
  -> InputFieldsParser m a
fieldWithDefault name description defaultValue parser = InputFieldsParser
  { ifDefinitions = [mkDefinition name description $ IFOptional (pType parser) (Just defaultValue)]
  , ifParser = M.lookup name >>> withPath (++[Key (unName name)]) . \case
      Just value -> peelVariableWith True expectedType value >>= parseValue expectedType
      Nothing    -> pInputParser parser $ GraphQLValue $ literal defaultValue
  }
  where
    expectedType = Just $ toGraphQLType $ pType parser
    parseValue _ value = pInputParser parser value
    {- See Note [Temporarily disabling query plan caching] in
    Hasura.GraphQL.Execute.Plan.

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
     -}

-- | Creates a parser for a nullable field with no default value. If the field
-- is omitted, the provided parser /will not be called/. This allows a field to
-- distinguish an omitted field from a field supplied with @null@ (which is
-- permitted by the GraphQL specification); see Note [Optional fields and
-- nullability] for more details.
--
-- If you want a field with a default value of @null@, combine 'field' with
-- 'nullable', instead.
fieldOptional
  :: (MonadParse m, 'Input <: k)
  => Name
  -> Maybe Description
  -> Parser k m a
  -> InputFieldsParser m (Maybe a)
fieldOptional name description parser = InputFieldsParser
  { ifDefinitions = [mkDefinition name description $
      IFOptional (nullableType $ pType parser) Nothing]
  , ifParser = M.lookup name >>> withPath (++[Key (unName name)]) .
               traverse (pInputParser parser <=< peelVariable expectedType)
  }
  where
    expectedType = Just $ toGraphQLType $ nullableType $ pType parser

-- | A variant of 'selectionSetObject' which doesn't implement any interfaces
selectionSet
  :: MonadParse m
  => Name
  -> Maybe Description
  -> [FieldParser m a]
  -> Parser 'Output m (OMap.InsOrdHashMap Name (ParsedSelection a))
selectionSet name desc fields = selectionSetObject name desc fields []

safeSelectionSet
  :: (MonadError QErr n, MonadParse m)
  => Name
  -> Maybe Description
  -> [FieldParser m a]
  -> n (Parser 'Output m (OMap.InsOrdHashMap Name (ParsedSelection a)))
safeSelectionSet name desc fields
  | S.null duplicates = pure $ selectionSetObject name desc fields []
  | otherwise         = throw500 $ "found duplicate fields in selection set: " <> commaSeparated (unName <$> toList duplicates)
  where
    duplicates = LE.duplicates $ getName . fDefinition <$> fields

-- Should this rather take a non-empty `FieldParser` list?
-- See also Note [Selectability of tables].
selectionSetObject
  :: MonadParse m
  => Name
  -> Maybe Description
  -> [FieldParser m a]
  -- ^ Fields of this object, including any fields that are required from the
  -- interfaces that it implements.  Note that we can't derive those fields from
  -- the list of interfaces (next argument), because the types of the fields of
  -- the object are only required to be *subtypes* of the types of the fields of
  -- the interfaces it implements.
  -> [Parser 'Output m b]
  -- ^ Interfaces implemented by this object;
  -- see Note [The interfaces story] in Hasura.GraphQL.Parser.Schema.
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

      -- TODO(PDV) This probably accepts invalid queries, namely queries that use
      -- type names that do not exist.
      fields <- collectFields (name:parsedInterfaceNames) (runParser boolean) input
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
    interfaces = mapMaybe (getInterfaceInfo . pType) implementsInterfaces
    parsedInterfaceNames = fmap getName interfaces

selectionSetInterface
  :: (MonadParse n, Traversable t)
  => Name
  -> Maybe Description
  -> [FieldParser n a]
  -- ^ Fields defined in this interface
  -> t (Parser 'Output n b)
  -- ^ Parsers for the object types that implement this interface; see
  -- Note [The interfaces story] in Hasura.GraphQL.Parser.Schema for details.
  -> Parser 'Output n (t b)
selectionSetInterface name description fields objectImplementations = Parser
  { pType = Nullable $ TNamed $ mkDefinition name description $
      TIInterface $ InterfaceInfo (map fDefinition fields) objects
  , pParser = \input -> for objectImplementations (($ input) . pParser)
  -- Note: This is somewhat suboptimal, since it parses a query against every
  -- possible object implementing this interface, possibly duplicating work for
  -- fields defined on the interface itself.
  --
  -- Furthermore, in our intended use case (Relay), based on a field argument,
  -- we can decide which object we are about to retrieve, so in theory we could
  -- save some work by only parsing against that object type. But it’s still
  -- useful to parse against all of them, since it checks the validity of any
  -- fragments on the other types.
  }
  where
    objects = catMaybes $ toList $ fmap (getObjectInfo . pType) objectImplementations

selectionSetUnion
  :: (MonadParse n, Traversable t)
  => Name
  -> Maybe Description
  -> t (Parser 'Output n b) -- ^ The member object types.
  -> Parser 'Output n (t b)
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
selection name description argumentsParser resultParser =
  rawSelection name description argumentsParser resultParser
  <&> \(_alias, _args, a) -> a

rawSelection
  :: forall m a b
   . MonadParse m
  => Name
  -> Maybe Description
  -> InputFieldsParser m a -- ^ parser for the input arguments
  -> Parser 'Both m b -- ^ type of the result
  -> FieldParser m (Maybe Name, HashMap Name (Value Variable), a)
  -- ^ alias provided (if any), and the arguments
rawSelection name description argumentsParser resultParser = FieldParser
  { fDefinition = mkDefinition name description $
      FieldInfo (ifDefinitions argumentsParser) (pType resultParser)
  , fParser = \Field{ _fAlias, _fArguments, _fSelectionSet } -> do
      unless (null _fSelectionSet) $
        parseError "unexpected subselection set for non-object field"
      -- check for extraneous arguments here, since the InputFieldsParser just
      -- handles parsing the fields it cares about
      for_ (M.keys _fArguments) \argumentName ->
        unless (argumentName `S.member` argumentNames) $
          parseError $ name <<> " has no argument named " <>> argumentName
      fmap (_fAlias, _fArguments, ) $ withPath (++[Key "args"]) $ ifParser argumentsParser $ GraphQLValue <$> _fArguments
  }
  where
    -- If  `ifDefinitions` is empty, then not forcing this will lead to
    -- a thunk which is usually never forced because the definition is only used
    -- inside the loop which checks arguments have the correct name.
    -- Forcing it will lead to the statically allocated empty set.
    -- If it's non-empty then it will be forced the first time the parser
    -- is used so might as well force it when constructing the parser.
    !argumentNames = S.fromList (dName <$> ifDefinitions argumentsParser)

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
subselection name description argumentsParser bodyParser =
  rawSubselection name description argumentsParser bodyParser
  <&> \(_alias, _args, a, b) -> (a, b)

rawSubselection
  :: forall m a b
   . MonadParse m
  => Name
  -> Maybe Description
  -> InputFieldsParser m a -- ^ parser for the input arguments
  -> Parser 'Output m b -- ^ parser for the subselection set
  -> FieldParser m (Maybe Name, HashMap Name (Value Variable), a, b)
rawSubselection name description argumentsParser bodyParser = FieldParser
  { fDefinition = mkDefinition name description $
      FieldInfo (ifDefinitions argumentsParser) (pType bodyParser)
  , fParser = \Field{ _fAlias, _fArguments, _fSelectionSet } -> do
      -- check for extraneous arguments here, since the InputFieldsParser just
      -- handles parsing the fields it cares about
      for_ (M.keys _fArguments) \argumentName ->
        unless (argumentName `S.member` argumentNames) $
          parseError $ name <<> " has no argument named " <>> argumentName
      (_fAlias,_fArguments,,) <$> withPath (++[Key "args"]) (ifParser argumentsParser $ GraphQLValue <$> _fArguments)
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

valueToJSON :: MonadParse m => GType -> InputValue Variable -> m A.Value
valueToJSON expected = peelVariable (Just expected) >=> valueToJSON'
  where
    valueToJSON' = \case
      JSONValue    j -> pure j
      GraphQLValue g -> graphQLToJSON g
    graphQLToJSON = \case
      VNull               -> pure A.Null
      VInt i              -> pure $ A.toJSON i
      VFloat f            -> pure $ A.toJSON f
      VString t           -> pure $ A.toJSON t
      VBoolean b          -> pure $ A.toJSON b
      VEnum (EnumValue n) -> pure $ A.toJSON n
      VList values        -> A.toJSON <$> traverse graphQLToJSON values
      VObject objects     -> A.toJSON <$> traverse graphQLToJSON objects
      VVariable variable  -> valueToJSON' $ absurd <$> vValue variable

jsonToGraphQL :: (MonadError Text m) => A.Value -> m (Value Void)
jsonToGraphQL = \case
  A.Null        -> pure VNull
  A.Bool val    -> pure $ VBoolean val
  A.String val  -> pure $ VString val
  A.Number val  -> case toBoundedInteger val of
    Just intVal -> pure $ VInt $ fromIntegral @Int64 intVal
    Nothing     -> pure $ VFloat val
  A.Array vals  -> VList <$> traverse jsonToGraphQL (toList vals)
  A.Object vals -> VObject . M.fromList <$> for (M.toList vals) \(key, val) -> do
    graphQLName <- onNothing (mkName key) $ throwError $
      "variable value contains object with key " <> key <<> ", which is not a legal GraphQL name"
    (graphQLName,) <$> jsonToGraphQL val

peelVariable :: MonadParse m => Maybe GType -> InputValue Variable -> m (InputValue Variable)
peelVariable = peelVariableWith False

peelVariableWith :: MonadParse m => Bool -> Maybe GType -> InputValue Variable -> m (InputValue Variable)
peelVariableWith hasLocationDefaultValue expected = \case
  GraphQLValue (VVariable var) -> do
    onJust expected \locationType -> typeCheck hasLocationDefaultValue locationType var
    markNotReusable
    pure $ absurd <$> vValue var
  value -> pure value

typeCheck :: MonadParse m => Bool -> GType -> Variable -> m ()
typeCheck hasLocationDefaultValue locationType variable@Variable { vInfo, vType } =
  unless (isVariableUsageAllowed hasLocationDefaultValue locationType variable) $ parseError
    $ "variable " <> dquote (getName vInfo) <> " is declared as "
    <> showGT vType <> ", but used where "
    <> showGT locationType <> " is expected"

typeMismatch :: MonadParse m => Name -> Text -> InputValue Variable -> m a
typeMismatch name expected given = parseError $
  "expected " <> expected <> " for type " <> name <<> ", but found " <> describeValue given

describeValue :: InputValue Variable -> Text
describeValue = describeValueWith (describeValueWith absurd . vValue)

describeValueWith :: (var -> Text) -> InputValue var -> Text
describeValueWith describeVariable = \case
  JSONValue    jval -> describeJSON    jval
  GraphQLValue gval -> describeGraphQL gval
  where
    describeJSON = \case
      A.Null     -> "null"
      A.Bool _   -> "a boolean"
      A.String _ -> "a string"
      A.Number _ -> "a number"
      A.Array  _ -> "a list"
      A.Object _ -> "an object"
    describeGraphQL = \case
      VVariable var -> describeVariable var
      VInt _        -> "an integer"
      VFloat _      -> "a float"
      VString _     -> "a string"
      VBoolean _    -> "a boolean"
      VNull         -> "null"
      VEnum _       -> "an enum value"
      VList _       -> "a list"
      VObject _     -> "an object"

-- | Checks whether the type of a variable is compatible with the type
--   at the location at which it is used. This is an implementation of
--   the function described in section 5.8.5 of the spec:
--   http://spec.graphql.org/June2018/#sec-All-Variable-Usages-are-Allowed
--   No input type coercion is allowed between variables: coercion
--   rules only allow when translating a value from a literal. It is
--   therefore not allowed to use an Int variable at a Float location,
--   despite the fact that it is legal to use an Int literal at a
--   Float location.
--   Furthermore, it's also worth noting that there's one tricky case
--   where we might allow a nullable variable at a non-nullable
--   location: when either side has a non-null default value. That's
--   because GraphQL conflates nullability and optinal fields (see
--   Note [Optional fields and nullability] for more details).
isVariableUsageAllowed
  :: Bool      -- ^ does the location have a default value
  -> GType     -- ^ the location type
  -> Variable  -- ^ the variable
  -> Bool
isVariableUsageAllowed hasLocationDefaultValue locationType variable
  | isNullable locationType       = areTypesCompatible locationType variableType
  | not $ isNullable variableType = areTypesCompatible locationType variableType
  | hasLocationDefaultValue       = areTypesCompatible locationType variableType
  | hasNonNullDefault variable    = areTypesCompatible locationType variableType
  | otherwise                     = False
  where
    areTypesCompatible = compareTypes `on` \case
      TypeNamed _ n -> TypeNamed (Nullability True) n
      TypeList  _ n -> TypeList  (Nullability True) n
    variableType = vType variable
    hasNonNullDefault = vInfo >>> \case
      VIRequired _       -> False
      VIOptional _ value -> value /= VNull
    compareTypes = curry \case
      (TypeList lNull lType, TypeList vNull vType)
        -> checkNull lNull vNull && areTypesCompatible lType vType
      (TypeNamed lNull lType, TypeNamed vNull vType)
        -> checkNull lNull vNull && lType == vType
      _ -> False
    checkNull (Nullability expectedNull) (Nullability actualNull) =
      expectedNull || not actualNull
