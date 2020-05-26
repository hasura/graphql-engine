{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StrictData          #-}

-- | Defines the 'Parser' type and its primitive combinators.
module Hasura.GraphQL.Parser.Internal.Parser where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended  as M
import qualified Data.HashMap.Strict.InsOrd    as OMap
import qualified Data.HashSet                  as S

import           Control.Lens.Extended         hiding (enum, index)
import           Data.GADT.Compare.Extended
import           Data.Int                      (Int32)
import           Data.Parser.JSONPath
import           Data.Type.Equality
import           Data.Void
import           Language.GraphQL.Draft.Syntax (Description (..), EnumValue (..), Field (..),
                                                Name (..), Selection (..), SelectionSet, Value (..),
                                                litName, literal)

import           Hasura.GraphQL.Parser.Class
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
  ParserInput 'Both = Value Variable -- see Note [The 'Both kind] in Hasura.GraphQL.Parser.Schema
  ParserInput 'Input = Value Variable
  ParserInput 'Output = SelectionSet Variable -- see Note [The meaning of Parser 'Output]

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

-- | Parses some collection of input fields. Build a 'FieldsParser' using
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
  , fParser     :: Field Variable -> m a
  } deriving (Functor)

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
  { pType = NonNullable $ TNamed $ mkDefinition name description TIScalar
  , pParser = peelVariable >=> \v -> case representation of
      SRBoolean -> case v of
        VBoolean a -> pure a
        _          -> typeMismatch name "a boolean" v
      SRInt -> case v of
        VInt a -> pure a
        _      -> typeMismatch name "an integer" v
      SRFloat -> case v of
        VFloat a -> pure a
        _        -> typeMismatch name "a float" v
      SRString -> case v of
        VString a -> pure a
        _         -> typeMismatch name "a string" v
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
  { pType = NonNullable $ TNamed $ mkDefinition name description $ TIEnum (fst <$> values)
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
      Nullable t    -> Nullable t
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
      -- TODO: handle input list coercion
      other -> parseError $ "expected a list, but found " <> describeValue other
  }

object
  :: MonadParse m
  => Name
  -> Maybe Description
  -> InputFieldsParser m a
  -> Parser 'Input m a
object name description parser = Parser
  { pType = NonNullable $ TNamed $ mkDefinition name description $
      TIInputObject (ifDefinitions parser)
  , pParser = peelVariable >=> \case
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
  -> InputFieldsParser m a
fieldWithDefault name description defaultValue parser = InputFieldsParser
  { ifDefinitions = [mkDefinition name description $
      IFOptional (discardNullability $ pType parser) (Just defaultValue)]
  , ifParser = M.lookup name >>> withPath (Key (unName name) :) . \case
      Just value -> parseValue value
      Nothing    -> pInputParser parser $ literal defaultValue
  }
  where
    parseValue = \value -> case value of
      VVariable (Variable { vInfo, vValue }) ->
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
          VIRequired _   -> parseValue value
          VIOptional _ _ -> markNotReusable *> parseValue (literal vValue)
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
  -> InputFieldsParser m (Maybe a)
fieldOptional name description parser = InputFieldsParser
  { ifDefinitions = [mkDefinition name description $
      IFOptional (discardNullability $ pType parser) Nothing]
  , ifParser = M.lookup name >>> withPath (Key (unName name) :) . traverse (pInputParser parser)
  }

-- Should this rather take a non-empty `FieldParser` list?
-- See also Note [Selectability of tables].
selectionSet
  :: MonadParse m
  => Name
  -> Maybe Description
  -> [FieldParser m a]
  -> Parser 'Output m (OMap.InsOrdHashMap Name (ParsedSelection a))
selectionSet name description parsers = Parser
  { pType = NonNullable $ TNamed $ mkDefinition name description $
      TIObject $ map fDefinition parsers
  , pParser = \input -> do
      let fields = input & mapMaybe \case
            -- FIXME: handle fragments
            SelectionField inputField -> Just inputField
            _ -> Nothing
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
      when (null fields) $ parseError $ "missing selection set for " <>> name

      OMap.fromList <$> for fields \selectionField@Field{ _fName, _fAlias } -> do
        parsedField <- if
          | _fName == $$(litName "__typename") ->
              pure $ SelectTypename name
          | Just parser <- M.lookup _fName parserMap ->
              withPath (Key (unName _fName) :) $
                SelectField <$> parser selectionField
          | otherwise ->
              parseError $ name <<> " has no field named " <>> _fName
        pure (fromMaybe _fName _fAlias, parsedField)
  }
  where
    parserMap = parsers
      & map (\FieldParser{ fDefinition, fParser } -> (getName fDefinition, fParser))
      & M.fromList

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
      ifParser argumentsParser _fArguments
  }

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
  -> Parser 'Output m b -- ^ parser for the subselection set; see also
  -> FieldParser m (a, b)
subselection name description argumentsParser bodyParser = FieldParser
  { fDefinition = mkDefinition name description $
      FieldInfo (ifDefinitions argumentsParser) (pType bodyParser)
  , fParser = \Field{ _fArguments, _fSelectionSet } ->
      (,) <$> ifParser argumentsParser _fArguments
          <*> pParser bodyParser _fSelectionSet
  }

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


-- -- | See 'selection'. Also see Note [The delicate balance of GraphQL kinds] in
-- -- "Hasura.GraphQL.Parser.Schema".
-- type family SelectionResult k a b = r | r -> k a where
--   SelectionResult 'Both   a _ = (Name, a)
--   SelectionResult 'Output a b = (Name, a, b)
--
-- -- | Constructs a parser for a field of a 'selectionSet'. Fields of a selection
-- -- set have two peculiarities:
-- --
-- --   1. They can have arguments, such as in @table(where: {field: {_eq: $blah}})@.
-- --
-- --   2. They have a sub-selection set /if and only if/ their result type is an
-- --      object type.
-- --
-- -- The first requirement is satisfied easily by providing a
-- -- @'FieldsParser' ''Input'@ to parse the arguments, but the second requirement
-- -- is more subtle. One might expect us to have two variants of 'selection':
-- --
-- --   1. A variant that accepts a @'Parser' ''Output'@ to parse a sub-selection
-- --      set.
-- --
-- --   2. Another variant that doesn’t accept a child 'Parser' at all and doesn’t
-- --      parse a sub-selection set.
-- --
-- -- However, that leaves us with a problem in case 2: how do we know what type
-- -- the field has? After all, we use the provided 'Parser' to build the GraphQL
-- -- schema in addition to parsing the GraphQL query.
-- --
-- -- We could solve this problem by passing in a @'Type' ''Both'@ instead of a
-- -- 'Parser', but generally the parsing code isn’t expected to deal with 'Type's
-- -- directly. Therefore, we just accept a @'Parser' ''Both'@, and we happen to
-- -- only use it for its type.
-- --
-- -- But this leaves us with /another/ problem: what is the result type of
-- -- 'selection'? If we have a sub-selection set, we must return three results:
-- -- the field alias, the parsed field arguments, and the parsed sub-selection
-- -- set. But if we /don’t/ have a sub-selection set, we have no third result to
-- -- return! That complication is handled by the 'SelectionResult' type family,
-- -- which computes how many results should be returned.
-- --
-- -- All quite subtle when you spell it out, but in practice, most of the above
-- -- can be ignored when actually writing parsers: just pass a 'Parser' for the
-- -- field’s result type, and everything “just works”.
-- --
-- -- Finally, a couple miscellaneous other details:
-- --
-- --   * All fields of a selection set are always optional according to the
-- --     GraphQL specification, so 'selection' always returns a 'Maybe'.
-- --
-- --   * It was alluded to above, but the 'Name' in the result is the field’s
-- --     /alias/, which must be used when constructing the query result, since it
-- --     may be different from the field name.
-- --
-- --   * If you have a field that takes no input arguments, use the 'selection_'
-- --     shorthand combinator.
-- selection
--   :: forall k m a b
--    . (MonadParse m, 'Output <: k)
--   => Name
--   -> Maybe Description
--   -> InputFieldsParser m a -- ^ parser for the input arguments
--   -> Parser k m b -- ^ parser for the result type
--   -> FieldsParser 'Output m (Maybe (SelectionResult k a b))
-- selection name description argumentsParser bodyParser = FieldsParser
--   { ifDefinitions = [mkDefinition name description $
--       FieldInfo (ifDefinitions argumentsParser) (pType bodyParser)]
--   , ifParser = \selectionFields -> withPath (Key (unName name) :) $
--       for (OMap.lookup name selectionFields) \selectionField -> do
--         let Field{ _fName = fieldName, _fArguments = arguments } = selectionField
--             alias = _fAlias selectionField & fromMaybe fieldName
--
--         for_ (M.keys arguments) \argumentName -> do
--           unless (argumentName `S.member` argumentNames) $
--             parseError $ name <<> " has no argument named " <>> argumentName
--         -- Here we use OMap.fromHashMap, which is a bit cheaty, since we’re
--         -- faking an ordered map from an unordered map. But the ordering isn’t
--         -- really important for input arguments.
--         parsedArguments <- ifParser argumentsParser (OMap.fromHashMap arguments)
--
--         -- see Note [The delicate balance of GraphQL kinds] in Hasura.GraphQL.Parser.Schema
--         case subKind @'Output @k of
--           KBoth -> pure (alias, parsedArguments)
--           KRefl -> do
--             parsedBody <- pParser bodyParser $ _fSelectionSet selectionField
--             pure (alias, parsedArguments, parsedBody)
--   }
--   where
--     argumentNames = S.fromList (dName <$> ifDefinitions argumentsParser)
--
-- -- | Analogous to 'SelectionResult', but for 'selection_'.
-- type family SelectionResult_ k a = r | r -> k where
--   SelectionResult_ 'Both   _ = Name
--   SelectionResult_ 'Output a = (Name, a)
--
-- -- | A shorthand for a 'selection' that takes no input arguments.
-- selection_
--   :: forall k m a
--    . (MonadParse m, 'Output <: k)
--   => Name
--   -> Maybe Description
--   -> Parser k m a
--   -> FieldsParser 'Output m (Maybe (SelectionResult_ k a))
-- selection_ name description parser = selection name description (pure ()) parser
--   -- see Note [The delicate balance of GraphQL kinds] in Hasura.GraphQL.Parser.Schema
--   <&> fmap case subKind @'Output @k of
--         KBoth -> \(alias, ()) -> alias
--         KRefl -> \(alias, (), result) -> (alias, result)

-- -----------------------------------------------------------------------------
-- internal helpers

peelVariable :: MonadParse m => Value Variable -> m (Value Variable)
peelVariable (VVariable (Variable { vValue })) = markNotReusable $> literal vValue
peelVariable value                             = pure value

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
