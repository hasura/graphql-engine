{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StrictData          #-}

-- | Defines the 'Parser' type and its primitive combinators.
module Hasura.GraphQL.Parser.Internal.Parser where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended  as M
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

-- -------------------------------------------------------------------------------------------------
-- type definitions

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
  , ifParser      :: HashMap Name (FieldInput k) -> m a
  } deriving (Functor)

infixl 1 `bindFields`
bindFields :: Monad m => FieldsParser k m a -> (a -> m b) -> FieldsParser k m b
bindFields p f = p { ifParser = ifParser p >=> f }

type family FieldInput k = r | r -> k where
  FieldInput 'Input = Value Variable
  FieldInput 'Output = Field Variable

instance Applicative m => Applicative (FieldsParser k m) where
  pure v = FieldsParser [] (const $ pure v)
  a <*> b = FieldsParser
    (ifDefinitions a <> ifDefinitions b)
    (liftA2 (<*>) (ifParser a) (ifParser b))

-- -------------------------------------------------------------------------------------------------
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
  -> FieldsParser 'Input m a
  -> Parser 'Input m a
object name description parser = Parser
  { pType = NonNullable $ TNamed $ mkDefinition name description $
      TIInputObject (ifDefinitions parser)
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
  -> FieldsParser 'Input m a
fieldWithDefault name description defaultValue parser = FieldsParser
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
  -> FieldsParser 'Input m (Maybe a)
fieldOptional name description parser = FieldsParser
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
  -> a
  -> FieldsParser 'Output m [a]
  -> Parser 'Output m [a]
selectionSet name description typenameRepr parser = Parser
  { pType = NonNullable $ TNamed $ mkDefinition name description $ TIObject (ifDefinitions parser)
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

      -- check for extraneous fields here, since the FieldsParser just
      -- handles parsing the fields it cares about
      for_ fields \Field{ _fName = fieldName } -> do
        unless (fieldName `S.member` fieldNames) $
          unless (fieldName == $$(litName"__typename")) $
          parseError $ name <<> " has no field named " <>> fieldName
      parsedFields <- ifParser parser $! M.fromListOn _fName fields

      -- __typename is a special case: while every selection set
      -- must accept it as a potential output field, it is not
      -- exposed as part of the schema
      pure $ [typenameRepr | any (== $$(litName "__typename")) $ _fName <$> fields] <> parsedFields
  }
  where
    fieldNames = S.fromList (dName <$> ifDefinitions parser)

-- | See 'selection'.
type family SelectionResult k a b = r | r -> k a where
  SelectionResult 'Both   a _ = (Name, a)
  SelectionResult 'Output a b = (Name, a, b)

-- | Constructs a parser for a field of a 'selectionSet'. Fields of a selection
-- set have two peculiarities:
--
--   1. They can have arguments, such as in @table(where: {field: {_eq: $blah}})@.
--
--   2. They have a sub-selection set /if and only if/ their result type is an
--      object type.
--
-- The first requirement is satisfied easily by providing a
-- @'FieldsParser' ''Input'@ to parse the arguments, but the second requirement
-- is more subtle. One might expect us to have two variants of 'selection':
--
--   1. A variant that accepts a @'Parser' ''Output'@ to parse a sub-selection
--      set.
--
--   2. Another variant that doesn’t accept a child 'Parser' at all and doesn’t
--      parse a sub-selection set.
--
-- However, that leaves us with a problem in case 2: how do we know what type
-- the field has? After all, we use the provided 'Parser' to build the GraphQL
-- schema in addition to parsing the GraphQL query.
--
-- We could solve this problem by passing in a @'Type' ''Both'@ instead of a
-- 'Parser', but generally the parsing code isn’t expected to deal with 'Type's
-- directly. Therefore, we just accept a @'Parser' ''Both'@, and we happen to
-- only use it for its type.
--
-- But this leaves us with /another/ problem: what is the result type of
-- 'selection'? If we have a sub-selection set, we must return three results:
-- the field alias, the parsed field arguments, and the parsed sub-selection
-- set. But if we /don’t/ have a sub-selection set, we have no third result to
-- return! That complication is handled by the 'SelectionResult' type family,
-- which computes how many results should be returned.
--
-- All quite subtle when you spell it out, but in practice, most of the above
-- can be ignored when actually writing parsers: just pass a 'Parser' for the
-- field’s result type, and everything “just works”.
--
-- Finally, a couple miscellaneous other details:
--
--   * All fields of a selection set are always optional according to the
--     GraphQL specification, so 'selection' always returns a 'Maybe'.
--
--   * It was alluded to above, but the 'Name' in the result is the field’s
--     /alias/, which must be used when constructing the query result, since it
--     may be different from the field name.
--
--   * If you have a field that takes no input arguments, use the 'selection_'
--     shorthand combinator.
selection
  :: forall k m a b
   . (MonadParse m, 'Output <: k)
  => Name
  -> Maybe Description
  -> FieldsParser 'Input m a -- ^ parser for the input arguments
  -> Parser k m b -- ^ parser for the result type
  -> FieldsParser 'Output m (Maybe (SelectionResult k a b))
selection name description argumentsParser bodyParser = FieldsParser
  { ifDefinitions = [mkDefinition name description $
      FieldInfo (ifDefinitions argumentsParser) (pType bodyParser)]
  , ifParser = M.lookup name >>> withPath (Key (unName name) :) . traverse \selectionField -> do
      let Field{ _fName = fieldName, _fArguments = arguments } = selectionField
          alias = _fAlias selectionField & fromMaybe fieldName

      for_ (M.keys arguments) \argumentName -> do
        unless (argumentName `S.member` argumentNames) $
          parseError $ name <<> " has no argument named " <>> argumentName
      parsedArguments <- ifParser argumentsParser arguments

      -- see Note [The delicate balance of GraphQL kinds] in Hasura.GraphQL.Parser.Schema
      case subKind @'Output @k of
        KBoth -> pure (alias, parsedArguments)
        KRefl -> do
          parsedBody <- pParser bodyParser $ _fSelectionSet selectionField
          pure (alias, parsedArguments, parsedBody)
  }
  where
    argumentNames = S.fromList (dName <$> ifDefinitions argumentsParser)

-- | Analogous to 'SelectionResult', but for 'selection_'.
type family SelectionResult_ k a = r | r -> k where
  SelectionResult_ 'Both   _ = Name
  SelectionResult_ 'Output a = (Name, a)

-- | A shorthand for a 'selection' that takes no input arguments.
selection_
  :: forall k m a
   . (MonadParse m, 'Output <: k)
  => Name
  -> Maybe Description
  -> Parser k m a
  -> FieldsParser 'Output m (Maybe (SelectionResult_ k a))
selection_ name description parser = selection name description (pure ()) parser
  -- see Note [The delicate balance of GraphQL kinds] in Hasura.GraphQL.Parser.Schema
  <&> fmap case subKind @'Output @k of
        KBoth -> \(alias, ()) -> alias
        KRefl -> \(alias, (), result) -> (alias, result)

-- -------------------------------------------------------------------------------------------------
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
