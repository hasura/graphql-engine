{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE StrictData #-}

-- | Defines the 'Parser' type and its primitive combinators.
module Hasura.GraphQL.Parser.Internal.Parser
  ( module Hasura.GraphQL.Parser.Internal.Parser
  , module Hasura.GraphQL.Parser.Internal.Input
  , Parser(..)
  , parserType
  , runParser
  , ParserInput
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                                  as A
import qualified Data.HashMap.Strict.Extended                as M
import qualified Data.HashMap.Strict.InsOrd                  as OMap
import qualified Data.HashSet                                as S
import qualified Data.List.Extended                          as LE

import           Data.Parser.JSONPath
import           Data.Text.Extended
import           Data.Type.Equality
import           Language.GraphQL.Draft.Syntax               hiding (Definition)

import           Hasura.Base.Error
import           Hasura.GraphQL.Parser.Class.Parse
import           Hasura.GraphQL.Parser.Collect
import           Hasura.GraphQL.Parser.Directives
import           Hasura.GraphQL.Parser.Internal.Input
import           Hasura.GraphQL.Parser.Internal.TypeChecking
import           Hasura.GraphQL.Parser.Internal.Types
import           Hasura.GraphQL.Parser.Schema


infixl 1 `bind`
bind :: Monad m => Parser k m a -> (a -> m b) -> Parser k m b
bind p f = p { pParser = pParser p >=> f }

infixl 1 `bindFields`
bindFields :: Monad m => InputFieldsParser m a -> (a -> m b) -> InputFieldsParser m b
bindFields p f = p { ifParser = ifParser p >=> f }

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

nullable :: forall k m a. (MonadParse m, 'Input <: k) => Parser k m a -> Parser k m (Maybe a)
nullable parser = gcastWith (inputParserInput @k) Parser
  { pType = schemaType
  , pParser = peelVariable (toGraphQLType schemaType) >=> \case
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

multipleField :: forall m a. FieldParser m a -> FieldParser m a
multipleField (FieldParser (Definition n u d (FieldInfo as t)) p) =
  FieldParser (Definition n u d (FieldInfo as (Nullable (TList t)))) p

-- | Decorate a schema output type as NON_NULL
nonNullableParser :: forall m a . Parser 'Output m a -> Parser 'Output m a
nonNullableParser parser = parser { pType = nonNullableType (pType parser) }

-- | Make a schema output as nullable
nullableParser :: forall m a . Parser 'Output m a -> Parser 'Output m a
nullableParser parser = parser { pType = nullableType (pType parser) }

multiple :: forall m a . Parser 'Output m a -> Parser 'Output m a
multiple parser = parser { pType = Nullable $ TList $ pType parser }


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
  | otherwise         = throw500 $ case desc of
      Nothing -> "found duplicate fields in selection set: " <> duplicatesList
      Just d  -> "found duplicate fields in selection set for " <> unDescription d <> ": " <> duplicatesList
  where
    duplicates = LE.duplicates $ getName . fDefinition <$> fields
    duplicatesList = commaSeparated $ unName <$> toList duplicates

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
      fields <- collectFields (name:parsedInterfaceNames) input
      for fields \selectionField@Field{ _fName, _fAlias, _fDirectives } -> do
        parsedValue <-
          if | _fName == $$(litName "__typename") ->
               pure $ SelectTypename name
             | Just parser <- M.lookup _fName parserMap ->
               withPath (++[Key (unName _fName)]) $
               SelectField <$> parser selectionField
             | otherwise ->
               withPath (++[Key (unName _fName)]) $
               parseError $ "field " <> _fName <<> " not found in type: " <> squote name
        _dirMap <- parseDirectives customDirectives (DLExecutable EDLFIELD) _fDirectives
        -- insert processing of custom directives here
        pure parsedValue

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
  -> InputFieldsParser m a  -- ^ parser for the input arguments
  -> Parser 'Output m b     -- ^ parser for the subselection set
  -> FieldParser m (a, b)
subselection name description argumentsParser bodyParser =
  rawSubselection name description argumentsParser bodyParser
  <&> \(_alias, _args, a, b) -> (a, b)

rawSubselection
  :: forall m a b
   . MonadParse m
  => Name
  -> Maybe Description
  -> InputFieldsParser m a  -- ^ parser for the input arguments
  -> Parser 'Output m b     -- ^ parser for the subselection set
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
