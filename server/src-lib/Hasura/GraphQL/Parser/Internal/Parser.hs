{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Defines the 'Parser' type and its primitive combinators.
module Hasura.GraphQL.Parser.Internal.Parser
  ( module Hasura.GraphQL.Parser.Internal.Parser,
    module Hasura.GraphQL.Parser.Internal.Input,
    Parser (..),
    FieldParser (..),
    parserType,
    runParser,
    ParserInput,
  )
where

import Control.Arrow ((&&&))
import Control.Monad (unless, when, (>=>))
import Control.Monad.Except (MonadError)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as K
import Data.Aeson.Types (JSONPathElement (Key))
import Data.Foldable (for_, toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.HashSet qualified as S
import Data.Hashable (Hashable)
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text.Extended
import Data.Traversable (for)
import Data.Type.Equality
import Hasura.Base.Error
import Hasura.GraphQL.Parser.Class.Parse
import Hasura.GraphQL.Parser.Collect
import Hasura.GraphQL.Parser.Directives
import Hasura.GraphQL.Parser.Internal.Input
import Hasura.GraphQL.Parser.Internal.TypeChecking
import Hasura.GraphQL.Parser.Internal.Types
import Hasura.GraphQL.Parser.Names
import Hasura.GraphQL.Parser.Schema
import Hasura.GraphQL.Parser.Variable
import Language.GraphQL.Draft.Syntax hiding (Definition)
import Language.GraphQL.Draft.Syntax qualified as G
import Prelude

infixl 1 `bind`

bind :: Monad m => Parser origin k m a -> (a -> m b) -> Parser origin k m b
bind p f = p {pParser = pParser p >=> f}

infixl 1 `bindFields`

bindFields :: Monad m => InputFieldsParser origin m a -> (a -> m b) -> InputFieldsParser origin m b
bindFields p f = p {ifParser = ifParser p >=> f}

-- | A parser for a single field in a selection set. Build a 'FieldParser'
-- with 'selection' or 'subselection', and combine them together with
-- 'selectionSet' to obtain a 'Parser'.
data FieldParser origin m a = FieldParser
  { fDefinition :: Definition origin (FieldInfo origin),
    fParser :: Field NoFragments Variable -> m a
  }
  deriving (Functor)

infixl 1 `bindField`

bindField :: Monad m => FieldParser origin m a -> (a -> m b) -> FieldParser origin m b
bindField p f = p {fParser = fParser p >=> f}

-- | A single parsed field in a selection set.
data ParsedSelection a
  = -- | An ordinary field.
    SelectField a
  | -- | The magical @__typename@ field, implicitly available on all objects
    -- <as part of GraphQL introspection http://spec.graphql.org/June2018/#sec-Type-Name-Introspection>.
    SelectTypename Name
  deriving (Functor)

handleTypename :: (Name -> a) -> ParsedSelection a -> a
handleTypename _ (SelectField value) = value
handleTypename f (SelectTypename name) = f name

nullable :: forall origin k m a. (MonadParse m, 'Input <: k) => Parser origin k m a -> Parser origin k m (Maybe a)
nullable parser =
  gcastWith
    (inputParserInput @k)
    Parser
      { pType = schemaType,
        pParser =
          peelVariable (toGraphQLType schemaType) >=> \case
            JSONValue A.Null -> pure Nothing
            GraphQLValue VNull -> pure Nothing
            value -> Just <$> pParser parser value
      }
  where
    schemaType = nullableType $ pType parser

-- | Decorate a schema field as NON_NULL
nonNullableField :: forall m origin a. FieldParser origin m a -> FieldParser origin m a
nonNullableField (FieldParser (Definition n d o (FieldInfo as t)) p) =
  FieldParser (Definition n d o (FieldInfo as (nonNullableType t))) p

-- | Decorate a schema field as NULL
nullableField :: forall m origin a. FieldParser origin m a -> FieldParser origin m a
nullableField (FieldParser (Definition n d o (FieldInfo as t)) p) =
  FieldParser (Definition n d o (FieldInfo as (nullableType t))) p

multipleField :: forall m origin a. FieldParser origin m a -> FieldParser origin m a
multipleField (FieldParser (Definition n d o (FieldInfo as t)) p) =
  FieldParser (Definition n d o (FieldInfo as (TList Nullable t))) p

-- | Decorate a schema field with reference to given @'G.GType'
wrapFieldParser :: forall m origin a. G.GType -> FieldParser origin m a -> FieldParser origin m a
wrapFieldParser = \case
  G.TypeNamed (G.Nullability True) _ -> nullableField
  G.TypeNamed (G.Nullability False) _ -> nonNullableField
  G.TypeList (G.Nullability True) t -> nullableField . multipleField . wrapFieldParser t
  G.TypeList (G.Nullability False) t -> nonNullableField . multipleField . wrapFieldParser t

-- | Decorate a schema output type as NON_NULL
nonNullableParser :: forall m origin a. Parser origin 'Output m a -> Parser origin 'Output m a
nonNullableParser parser = parser {pType = nonNullableType (pType parser)}

-- | Make a schema output as nullable
nullableParser :: forall m origin a. Parser origin 'Output m a -> Parser origin 'Output m a
nullableParser parser = parser {pType = nullableType (pType parser)}

multiple :: forall m origin a. Parser origin 'Output m a -> Parser origin 'Output m a
multiple parser = parser {pType = TList Nullable $ pType parser}

-- | Set the metadata origin of a 'Definition'
setDefinitionOrigin :: origin -> Definition origin a -> Definition origin a
setDefinitionOrigin o def = def {dOrigin = Just o}

-- | Set the metadata origin of a 'Parser'
setParserOrigin :: forall origin k m a. origin -> Parser origin k m a -> Parser origin k m a
setParserOrigin o (Parser typ p) =
  Parser (onTypeDef (setDefinitionOrigin o) typ) p

-- | Set the metadata origin of a 'FieldParser'
setFieldParserOrigin :: forall m origin a. origin -> FieldParser origin m a -> FieldParser origin m a
setFieldParserOrigin o (FieldParser (Definition n d _ i) p) =
  FieldParser (Definition n d (Just o) i) p

-- | Set the metadata origin of the arguments in a 'InputFieldsParser'
setInputFieldsParserOrigin :: forall m origin a. origin -> InputFieldsParser origin m a -> InputFieldsParser origin m a
setInputFieldsParserOrigin o (InputFieldsParser defs p) =
  InputFieldsParser (map (setDefinitionOrigin o) defs) p

-- | A variant of 'selectionSetObject' which doesn't implement any interfaces
selectionSet ::
  MonadParse m =>
  Name ->
  Maybe Description ->
  [FieldParser origin m a] ->
  Parser origin 'Output m (OMap.InsOrdHashMap Name (ParsedSelection a))
selectionSet name desc fields = selectionSetObject name desc fields []

safeSelectionSet ::
  forall n m origin a.
  (MonadError QErr n, MonadParse m, Eq origin, Hashable origin) =>
  (origin -> Text) ->
  Name ->
  Maybe Description ->
  [FieldParser origin m a] ->
  n (Parser origin 'Output m (OMap.InsOrdHashMap Name (ParsedSelection a)))
safeSelectionSet printOrigin name desc fields
  | null duplicates = pure $ selectionSetObject name desc fields []
  | otherwise = throw500 $ case desc of
    Nothing -> "found duplicate fields in selection set: " <> duplicatesList
    Just d -> "found duplicate fields in selection set for " <> unDescription d <> ": " <> duplicatesList
  where
    namesOrigins :: HashMap Name [Maybe origin]
    namesOrigins = M.fromListWith (<>) $ (dName &&& (pure . dOrigin)) . fDefinition <$> fields
    duplicates :: HashMap Name [Maybe origin]
    duplicates = M.filter ((> 1) . length) namesOrigins
    uniques = S.toList . S.fromList
    printEntry (fieldName, originsM) =
      let origins = uniques $ Maybe.catMaybes originsM
       in if
              | null origins -> unName fieldName
              | any Maybe.isNothing originsM ->
                unName fieldName <> " (generated for " <> commaSeparated (map printOrigin origins)
                  <> " and of unknown origin)"
              | otherwise ->
                unName fieldName <> " (generated for " <> commaSeparated (map printOrigin origins) <> ")"
    duplicatesList = commaSeparated $ printEntry <$> M.toList duplicates

-- Should this rather take a non-empty `FieldParser` list?
-- See also Note [Selectability of tables].
selectionSetObject ::
  MonadParse m =>
  Name ->
  Maybe Description ->
  -- | Fields of this object, including any fields that are required from the
  -- interfaces that it implements.  Note that we can't derive those fields from
  -- the list of interfaces (next argument), because the types of the fields of
  -- the object are only required to be *subtypes* of the types of the fields of
  -- the interfaces it implements.
  [FieldParser origin m a] ->
  -- | Interfaces implemented by this object;
  -- see Note [The interfaces story] in Hasura.GraphQL.Parser.Schema.
  [Parser origin 'Output m b] ->
  Parser origin 'Output m (OMap.InsOrdHashMap Name (ParsedSelection a))
selectionSetObject name description parsers implementsInterfaces =
  Parser
    { pType =
        TNamed Nullable $
          Definition name description Nothing $
            TIObject $ ObjectInfo (map fDefinition parsers) interfaces,
      pParser = \input -> withKey (Key "selectionSet") do
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
        fields <- collectFields (getName name : parsedInterfaceNames) input
        for fields \selectionField@Field {_fName, _fAlias, _fDirectives} -> do
          parsedValue <-
            if
                | _fName == $$(litName "__typename") ->
                  pure $ SelectTypename $ getName name
                | Just parser <- M.lookup _fName parserMap ->
                  withKey (Key (K.fromText (unName _fName))) $
                    SelectField <$> parser selectionField
                | otherwise ->
                  withKey (Key (K.fromText (unName _fName))) $
                    parseError $ "field " <> _fName <<> " not found in type: " <> squote name
          _dirMap <- parseDirectives customDirectives (DLExecutable EDLFIELD) _fDirectives
          -- insert processing of custom directives here
          pure parsedValue
    }
  where
    parserMap =
      parsers
        & map (\FieldParser {fDefinition, fParser} -> (getName fDefinition, fParser))
        & M.fromList
    interfaces = Maybe.mapMaybe (getInterfaceInfo . pType) implementsInterfaces
    parsedInterfaceNames = fmap getName interfaces

selectionSetInterface ::
  (MonadParse n, Traversable t) =>
  Name ->
  Maybe Description ->
  -- | Fields defined in this interface
  [FieldParser origin n a] ->
  -- | Parsers for the object types that implement this interface; see
  -- Note [The interfaces story] in Hasura.GraphQL.Parser.Schema for details.
  t (Parser origin 'Output n b) ->
  Parser origin 'Output n (t b)
selectionSetInterface name description fields objectImplementations =
  Parser
    { pType =
        TNamed Nullable $
          Definition name description Nothing $
            TIInterface $ InterfaceInfo (map fDefinition fields) objects,
      pParser = \input -> for objectImplementations (($ input) . pParser)
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
    objects = Maybe.catMaybes $ toList $ fmap (getObjectInfo . pType) objectImplementations

selectionSetUnion ::
  (MonadParse n, Traversable t) =>
  Name ->
  Maybe Description ->
  -- | The member object types.
  t (Parser origin 'Output n b) ->
  Parser origin 'Output n (t b)
selectionSetUnion name description objectImplementations =
  Parser
    { pType =
        TNamed Nullable $
          Definition name description Nothing $
            TIUnion $ UnionInfo objects,
      pParser = \input -> for objectImplementations (($ input) . pParser)
    }
  where
    objects = Maybe.catMaybes $ toList $ fmap (getObjectInfo . pType) objectImplementations

-- | Builds a 'FieldParser' for a field that does not take a subselection set,
-- i.e. a field that returns a scalar or enum. The field’s type is taken from
-- the provided 'Parser', but the 'Parser' is not otherwise used.
--
-- See also Note [The delicate balance of GraphQL kinds] in "Hasura.GraphQL.Parser.Schema".
selection ::
  forall m origin a b.
  MonadParse m =>
  Name ->
  Maybe Description ->
  -- | parser for the input arguments
  InputFieldsParser origin m a ->
  -- | type of the result
  Parser origin 'Both m b ->
  FieldParser origin m a
selection name description argumentsParser resultParser =
  rawSelection name description argumentsParser resultParser
    <&> \(_alias, _args, a) -> a

rawSelection ::
  forall m origin a b.
  MonadParse m =>
  Name ->
  Maybe Description ->
  -- | parser for the input arguments
  InputFieldsParser origin m a ->
  -- | type of the result
  Parser origin 'Both m b ->
  -- | alias provided (if any), and the arguments
  FieldParser origin m (Maybe Name, HashMap Name (Value Variable), a)
rawSelection name description argumentsParser resultParser =
  FieldParser
    { fDefinition =
        Definition name description Nothing $
          FieldInfo (ifDefinitions argumentsParser) (pType resultParser),
      fParser = \Field {_fAlias, _fArguments, _fSelectionSet} -> do
        unless (null _fSelectionSet) $
          parseError "unexpected subselection set for non-object field"
        -- check for extraneous arguments here, since the InputFieldsParser just
        -- handles parsing the fields it cares about
        for_ (M.keys _fArguments) \argumentName ->
          unless (argumentName `S.member` argumentNames) $
            parseError $ name <<> " has no argument named " <>> argumentName
        fmap (_fAlias,_fArguments,) $ withKey (Key "args") $ ifParser argumentsParser $ GraphQLValue <$> _fArguments
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
-- For example, @subselection name _ args fields@ produces schema:
--
-- > name (args) { fields }
--
-- See also Note [The delicate balance of GraphQL kinds] in "Hasura.GraphQL.Parser.Schema".
subselection ::
  forall m origin a b.
  MonadParse m =>
  Name ->
  Maybe Description ->
  -- | parser for the input arguments
  InputFieldsParser origin m a ->
  -- | parser for the subselection set
  Parser origin 'Output m b ->
  FieldParser origin m (a, b)
subselection name description argumentsParser bodyParser =
  rawSubselection name description argumentsParser bodyParser
    <&> \(_alias, _args, a, b) -> (a, b)

rawSubselection ::
  forall m origin a b.
  MonadParse m =>
  Name ->
  Maybe Description ->
  -- | parser for the input arguments
  InputFieldsParser origin m a ->
  -- | parser for the subselection set
  Parser origin 'Output m b ->
  FieldParser origin m (Maybe Name, HashMap Name (Value Variable), a, b)
rawSubselection name description argumentsParser bodyParser =
  FieldParser
    { fDefinition =
        Definition name description Nothing $
          FieldInfo (ifDefinitions argumentsParser) (pType bodyParser),
      fParser = \Field {_fAlias, _fArguments, _fSelectionSet} -> do
        -- check for extraneous arguments here, since the InputFieldsParser just
        -- handles parsing the fields it cares about
        for_ (M.keys _fArguments) \argumentName ->
          unless (argumentName `S.member` argumentNames) $
            parseError $ name <<> " has no argument named " <>> argumentName
        (_fAlias,_fArguments,,) <$> withKey (Key "args") (ifParser argumentsParser $ GraphQLValue <$> _fArguments)
          <*> pParser bodyParser _fSelectionSet
    }
  where
    argumentNames = S.fromList (dName <$> ifDefinitions argumentsParser)

-- | A shorthand for a 'selection' that takes no arguments.
selection_ ::
  MonadParse m =>
  Name ->
  Maybe Description ->
  -- | type of the result
  Parser origin 'Both m a ->
  FieldParser origin m ()
selection_ name description = selection name description (pure ())

-- | A shorthand for a 'subselection' that takes no arguments.
subselection_ ::
  MonadParse m =>
  Name ->
  Maybe Description ->
  -- | parser for the subselection set
  Parser origin 'Output m a ->
  FieldParser origin m a
subselection_ name description bodyParser =
  snd <$> subselection name description (pure ()) bodyParser
