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
import Control.Monad.Except (MonadError (throwError))
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.Types (JSONPathElement (Key))
import Data.Foldable (for_, toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashSet qualified as S
import Data.Hashable (Hashable)
import Data.Maybe qualified as Maybe
import Data.Traversable (for)
import Data.Type.Equality
import Data.Void (Void)
import Hasura.Base.ErrorMessage
import Hasura.Base.ToErrorValue
import Hasura.GraphQL.Parser.Class
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
import Witherable (catMaybes, mapMaybe)

infixl 1 `bind`

bind :: (Monad m) => Parser origin k m a -> (a -> m b) -> Parser origin k m b
bind p f = p {pParser = pParser p >=> f}

infixl 1 `bindFields`

bindFields :: (Monad m) => InputFieldsParser origin m a -> (a -> m b) -> InputFieldsParser origin m b
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

bindField :: (Monad m) => FieldParser origin m a -> (a -> m b) -> FieldParser origin m b
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

data NullableInput a
  = NullableInputValue a
  | NullableInputNull G.GType
  | NullableInputAbsent
  deriving (Show, Functor)

nullableToMaybe :: NullableInput a -> Maybe a
nullableToMaybe = fromNullableInput Nothing . (Just <$>)

fromNullableInput :: a -> NullableInput a -> a
fromNullableInput _ (NullableInputValue x) = x
fromNullableInput d _ = d

nullable :: forall origin k m a. (MonadParse m, 'Input <: k) => Parser origin k m a -> Parser origin k m (Maybe a)
nullable = fmap nullableToMaybe . nullableExact

-- | Distinguishes between inputs with an explicit null value, and inputs whose
-- variable value is absent without default.  See GraphQL spec June 2018 section
-- 2.9.5.
nullableExact :: forall origin k m a. (MonadParse m, 'Input <: k) => Parser origin k m a -> Parser origin k m (NullableInput a)
nullableExact parser =
  gcastWith
    (inputParserInput @k)
    Parser
      { pType = schemaType,
        pParser =
          peelVariableWith False gType >=> \case
            Just (JSONValue J.Null) -> pure $ NullableInputNull gType
            Just (GraphQLValue VNull) -> pure $ NullableInputNull gType
            Just value -> NullableInputValue <$> pParser parser value
            Nothing -> pure NullableInputAbsent
      }
  where
    schemaType = nullableType $ pType parser
    gType = toGraphQLType schemaType

-- | Decorate a schema field as NON_NULL
nonNullableField :: forall m origin a. FieldParser origin m a -> FieldParser origin m a
nonNullableField (FieldParser (Definition n d o dLst (FieldInfo as t)) p) =
  FieldParser (Definition n d o dLst (FieldInfo as (nonNullableType t))) p

-- | Decorate a schema field as NULL
nullableField :: forall m origin a. FieldParser origin m a -> FieldParser origin m a
nullableField (FieldParser (Definition n d o dLst (FieldInfo as t)) p) =
  FieldParser (Definition n d o dLst (FieldInfo as (nullableType t))) p

multipleField :: forall m origin a. FieldParser origin m a -> FieldParser origin m a
multipleField (FieldParser (Definition n d o dLst (FieldInfo as t)) p) =
  FieldParser (Definition n d o dLst (FieldInfo as (TList Nullable t))) p

-- | Decorate a schema field with reference to given @'G.GType'
wrapFieldParser :: forall m origin a. G.GType -> FieldParser origin m a -> FieldParser origin m a
wrapFieldParser = \case
  G.TypeNamed (G.Nullability True) _ -> nullableField
  G.TypeNamed (G.Nullability False) _ -> nonNullableField
  G.TypeList (G.Nullability True) t -> nullableField . multipleField . wrapFieldParser t
  G.TypeList (G.Nullability False) t -> nonNullableField . multipleField . wrapFieldParser t

-- | Decorate a schema type as NON_NULL.  Note that this is unsafe for
-- 'Output parsers, in the sense that 'nonNullableParser' doesn't (and due to
-- the polymorphic nature of the 'a' type parameter, can't) ensure that the
-- provided parser provides a semantically non-null value.
nonNullableParser :: forall m origin k a. Parser origin k m a -> Parser origin k m a
nonNullableParser parser = parser {pType = nonNullableType (pType parser)}

-- | Mark a schema type as nullable.  Syntactically speaking, this is the
-- default, because the GraphQL spec explicitly requires use of ! to mark types
-- as non-nullable.  But many constructions in our codebase are non-nullable by
-- default, since this matches the Haskell type system better.
--
-- Note that this is unsafe for 'Input parsers, in the sense that
-- 'nullableParser' doesn't ensure that the provided parser actually deals with
-- 'null' input values.
nullableParser :: forall m origin k a. Parser origin k m a -> Parser origin k m a
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
setFieldParserOrigin o (FieldParser (Definition n d _ dLst i) p) =
  FieldParser (Definition n d (Just o) dLst i) p

-- | Set the metadata origin of the arguments in a 'InputFieldsParser'
setInputFieldsParserOrigin :: forall m origin a. origin -> InputFieldsParser origin m a -> InputFieldsParser origin m a
setInputFieldsParserOrigin o (InputFieldsParser defs p) =
  InputFieldsParser (map (setDefinitionOrigin o) defs) p

-- | Set the directives of a 'Definition'
setDefinitionDirectives :: [G.Directive Void] -> Definition origin a -> Definition origin a
setDefinitionDirectives dLst def = def {dDirectives = dLst}

-- | Set the directives of a 'Parser'
setParserDirectives :: forall origin k m a. [G.Directive Void] -> Parser origin k m a -> Parser origin k m a
setParserDirectives dLst (Parser typ p) =
  Parser (onTypeDef (setDefinitionDirectives dLst) typ) p

-- | Set the directives of a 'FieldParser'
setFieldParserDirectives :: forall m origin a. [G.Directive Void] -> FieldParser origin m a -> FieldParser origin m a
setFieldParserDirectives dLst (FieldParser (Definition n d o _ i) p) =
  FieldParser (Definition n d o dLst i) p

-- | Set the directives of the arguments in a 'InputFieldsParser'
setInputFieldsParserDirectives :: forall m origin a. [G.Directive Void] -> InputFieldsParser origin m a -> InputFieldsParser origin m a
setInputFieldsParserDirectives dLst (InputFieldsParser defs p) =
  InputFieldsParser (map (setDefinitionDirectives dLst) defs) p

-- | A variant of 'selectionSetObject' which doesn't implement any interfaces
selectionSet ::
  (MonadParse m) =>
  Name ->
  Maybe Description ->
  [FieldParser origin m a] ->
  Parser origin 'Output m (InsOrdHashMap Name (ParsedSelection a))
selectionSet name desc fields = selectionSetObject name desc fields []

safeSelectionSet ::
  forall n m origin a.
  (MonadError ErrorMessage n, MonadParse m, Hashable origin, ToErrorValue origin) =>
  Name ->
  Maybe Description ->
  [FieldParser origin m a] ->
  n (Parser origin 'Output m (InsOrdHashMap Name (ParsedSelection a)))
{-# INLINE safeSelectionSet #-}
safeSelectionSet name description fields =
  case duplicatesList of
    [] -> pure $ selectionSetObject name description fields []
    -- singular
    [duplicate] -> throwError $ "Encountered conflicting definitions in the selection set for " <> toErrorValue name <> " for field " <> duplicate <> ". Fields must not be defined more than once across all sources."
    -- plural
    printedDuplicates -> throwError $ "Encountered conflicting definitions in the selection set for " <> toErrorValue name <> " for fields: " <> toErrorValue printedDuplicates <> ". Fields must not be defined more than once across all sources."
  where
    namesOrigins :: InsOrdHashMap Name [Maybe origin]
    namesOrigins =
      foldr
        (uncurry (InsOrdHashMap.insertWith (<>)))
        InsOrdHashMap.empty
        ((dName &&& (pure . dOrigin)) . fDefinition <$> fields)
    duplicates :: InsOrdHashMap Name [Maybe origin]
    duplicates = InsOrdHashMap.filter ((> 1) . length) namesOrigins
    uniques = S.toList . S.fromList
    printEntry (fieldName, originsM) =
      let origins = uniques $ catMaybes originsM
       in if
            | null origins -> toErrorValue fieldName
            | any Maybe.isNothing originsM ->
                toErrorValue fieldName <> " defined in " <> toErrorValue origins <> " and of unknown origin"
            | otherwise ->
                toErrorValue fieldName <> " defined in " <> toErrorValue origins
    duplicatesList = printEntry <$> InsOrdHashMap.toList duplicates

-- Should this rather take a non-empty `FieldParser` list?
-- See also Note [Selectability of tables].
selectionSetObject ::
  (MonadParse m) =>
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
  Parser origin 'Output m (InsOrdHashMap.InsOrdHashMap Name (ParsedSelection a))
{-# INLINE selectionSetObject #-}
selectionSetObject name description parsers implementsInterfaces =
  Parser
    { pType =
        TNamed Nullable $
          Definition name description Nothing [] $
            TIObject $
              ObjectInfo (map fDefinition parsers) interfaces,
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
          parseError $
            "missing selection set for " <> toErrorValue name

        -- TODO(PDV) This probably accepts invalid queries, namely queries that use
        -- type names that do not exist.
        fields <- collectFields (getName name : parsedInterfaceNames) input
        for fields \selectionField@Field {_fName, _fAlias, _fDirectives} -> do
          parsedValue <-
            if
              | _fName == $$(litName "__typename") ->
                  pure $ SelectTypename $ getName name
              | Just parser <- HashMap.lookup _fName parserMap ->
                  withKey (Key (K.fromText (unName _fName))) $
                    SelectField <$> parser selectionField
              | otherwise ->
                  withKey (Key (K.fromText (unName _fName))) $
                    parseError $
                      "field " <> toErrorValue _fName <> " not found in type: " <> toErrorValue name
          _dirMap <- parseDirectives customDirectives (DLExecutable EDLFIELD) _fDirectives
          -- insert processing of custom directives here
          pure parsedValue
    }
  where
    parserMap =
      parsers
        & map (\FieldParser {fDefinition, fParser} -> (getName fDefinition, fParser))
        & HashMap.fromList
    interfaces = mapMaybe (getInterfaceInfo . pType) implementsInterfaces
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
{-# INLINE selectionSetInterface #-}
selectionSetInterface name description fields objectImplementations =
  Parser
    { pType =
        TNamed Nullable $
          Definition name description Nothing [] $
            TIInterface $
              InterfaceInfo (map fDefinition fields) objects,
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
    objects = catMaybes $ toList $ fmap (getObjectInfo . pType) objectImplementations

selectionSetUnion ::
  (MonadParse n, Traversable t) =>
  Name ->
  Maybe Description ->
  -- | The member object types.
  t (Parser origin 'Output n b) ->
  Parser origin 'Output n (t b)
{-# INLINE selectionSetUnion #-}
selectionSetUnion name description objectImplementations =
  Parser
    { pType =
        TNamed Nullable $
          Definition name description Nothing [] $
            TIUnion $
              UnionInfo objects,
      pParser = \input -> for objectImplementations (($ input) . pParser)
    }
  where
    objects = catMaybes $ toList $ fmap (getObjectInfo . pType) objectImplementations

-- | Builds a 'FieldParser' for a field that does not take a subselection set,
-- i.e. a field that returns a scalar or enum. The field’s type is taken from
-- the provided 'Parser', but the 'Parser' is not otherwise used.
--
-- See also Note [The delicate balance of GraphQL kinds] in "Hasura.GraphQL.Parser.Schema".
selection ::
  forall m origin a b.
  (MonadParse m) =>
  Name ->
  Maybe Description ->
  -- | parser for the input arguments
  InputFieldsParser origin m a ->
  -- | type of the result
  Parser origin 'Both m b ->
  FieldParser origin m a
{-# INLINE selection #-}
selection name description argumentsParser resultParser =
  rawSelection name description argumentsParser resultParser
    <&> \(_alias, _args, a) -> a

rawSelection ::
  forall m origin a b.
  (MonadParse m) =>
  Name ->
  Maybe Description ->
  -- | parser for the input arguments
  InputFieldsParser origin m a ->
  -- | type of the result
  Parser origin 'Both m b ->
  -- | alias provided (if any), and the arguments
  FieldParser origin m (Maybe Name, HashMap Name (Value Variable), a)
{-# INLINE rawSelection #-}
rawSelection name description argumentsParser resultParser =
  FieldParser
    { fDefinition =
        Definition name description Nothing [] $
          FieldInfo (ifDefinitions argumentsParser) (pType resultParser),
      fParser = \Field {_fAlias, _fArguments, _fSelectionSet} -> do
        unless (null _fSelectionSet) $
          parseError "unexpected subselection set for non-object field"
        -- check for extraneous arguments here, since the InputFieldsParser just
        -- handles parsing the fields it cares about
        for_ (HashMap.keys _fArguments) \argumentName ->
          unless (argumentName `S.member` argumentNames) $
            parseError $
              toErrorValue name <> " has no argument named " <> toErrorValue argumentName
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
  (MonadParse m) =>
  Name ->
  Maybe Description ->
  -- | parser for the input arguments
  InputFieldsParser origin m a ->
  -- | parser for the subselection set
  Parser origin 'Output m b ->
  FieldParser origin m (a, b)
{-# INLINE subselection #-}
subselection name description argumentsParser bodyParser =
  rawSubselection name description argumentsParser bodyParser
    <&> \(_alias, _args, a, b) -> (a, b)

rawSubselection ::
  forall m origin a b.
  (MonadParse m) =>
  Name ->
  Maybe Description ->
  -- | parser for the input arguments
  InputFieldsParser origin m a ->
  -- | parser for the subselection set
  Parser origin 'Output m b ->
  FieldParser origin m (Maybe Name, HashMap Name (Value Variable), a, b)
{-# INLINE rawSubselection #-}
rawSubselection name description argumentsParser bodyParser =
  FieldParser
    { fDefinition =
        Definition name description Nothing [] $
          FieldInfo (ifDefinitions argumentsParser) (pType bodyParser),
      fParser = \Field {_fAlias, _fArguments, _fSelectionSet} -> do
        -- check for extraneous arguments here, since the InputFieldsParser just
        -- handles parsing the fields it cares about
        for_ (HashMap.keys _fArguments) \argumentName ->
          unless (argumentName `S.member` argumentNames) $
            parseError $
              toErrorValue name <> " has no argument named " <> toErrorValue argumentName
        (_fAlias,_fArguments,,)
          <$> withKey (Key "args") (ifParser argumentsParser $ GraphQLValue <$> _fArguments)
          <*> pParser bodyParser _fSelectionSet
    }
  where
    argumentNames = S.fromList (dName <$> ifDefinitions argumentsParser)

-- | A shorthand for a 'selection' that takes no arguments.
selection_ ::
  (MonadParse m) =>
  Name ->
  Maybe Description ->
  -- | type of the result
  Parser origin 'Both m a ->
  FieldParser origin m ()
{-# INLINE selection_ #-}
selection_ name description = selection name description (pure ())

-- | A shorthand for a 'subselection' that takes no arguments.
subselection_ ::
  (MonadParse m) =>
  Name ->
  Maybe Description ->
  -- | parser for the subselection set
  Parser origin 'Output m a ->
  FieldParser origin m a
{-# INLINE subselection_ #-}
subselection_ name description bodyParser =
  snd <$> subselection name description (pure ()) bodyParser
