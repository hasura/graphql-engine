{-# LANGUAGE StrictData #-}

-- | Defines the 'Parser' type and its primitive combinators.
module Hasura.GraphQL.Parser.Internal.Input where

import           Hasura.Prelude

import qualified Data.Aeson                                  as A
import qualified Data.HashMap.Strict.Extended                as M
import qualified Data.HashSet                                as S

import           Control.Lens.Extended                       hiding (enum, index)
import           Data.Parser.JSONPath
import           Data.Text.Extended
import           Data.Type.Equality
import           Language.GraphQL.Draft.Syntax               hiding (Definition)

import           Hasura.GraphQL.Parser.Class.Parse
import           Hasura.GraphQL.Parser.Internal.TypeChecking
import           Hasura.GraphQL.Parser.Internal.Types
import           Hasura.GraphQL.Parser.Schema
import           Hasura.Server.Utils                         (englishList)


-- ure that out on its own, so we have to be explicit to give
-- it a little help.
inputParserInput :: forall k. 'Input <: k => ParserInput k :~: InputValue Variable
inputParserInput = case subKind @'Input @k of { KRefl -> Refl; KBoth -> Refl }

pInputParser :: forall k m a. 'Input <: k => Parser k m a -> InputValue Variable -> m a
pInputParser = gcastWith (inputParserInput @k) pParser


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


instance Applicative m => Applicative (InputFieldsParser m) where
  pure v = InputFieldsParser [] (const $ pure v)
  a <*> b = InputFieldsParser
    (ifDefinitions a <> ifDefinitions b)
    (liftA2 (<*>) (ifParser a) (ifParser b))

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
    expectedType = toGraphQLType $ nullableType $ pType parser

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
    expectedType = toGraphQLType $ pType parser
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


-- -----------------------------------------------------------------------------
-- combinators

enum
  :: MonadParse m
  => Name
  -> Maybe Description
  -> NonEmpty (Definition EnumValueInfo, a)
  -> Parser 'Both m a
enum name description values = Parser
  { pType = schemaType
  , pParser = peelVariable (toGraphQLType schemaType) >=> \case
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

-- -----------------------------------------------------------------------------
-- helpers

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
  , pParser = peelVariable (toGraphQLType schemaType) >=> \case
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

list :: forall k m a. (MonadParse m, 'Input <: k) => Parser k m a -> Parser k m [a]
list parser = gcastWith (inputParserInput @k) Parser
  { pType = schemaType
  , pParser = peelVariable (toGraphQLType schemaType) >=> \case
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
