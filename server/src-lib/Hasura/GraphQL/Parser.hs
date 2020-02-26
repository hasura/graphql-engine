{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}

module Hasura.GraphQL.Parser where

import Hasura.Prelude

import qualified Data.HashMap.Strict.Extended as M
import qualified Data.HashSet as S

import Language.GraphQL.Draft.Syntax (Description(..), EnumValue(..), Name(..), NamedType(..), Value(..), StringValue(..), Field(..), SelectionSet, Selection(..), ValueConst(..), ObjectValueG(..), ObjectFieldG(..), ListValueG(..), Alias(..), Argument(..))
import Data.Int (Int32)
import Control.Lens.Extended hiding (enum)
import Data.Type.Equality

import qualified Hasura.RQL.Types as RQL

import Hasura.SQL.Types
import Hasura.SQL.Value
import Hasura.Server.Utils (englishList)
import Hasura.RQL.Types hiding (EnumValue(..), EnumValueInfo(..), FieldInfo(..))
import Hasura.GraphQL.Resolve.Types (UnresolvedVal(..), AnnPGVal(..))
import Hasura.GraphQL.Schema.Common

literal :: ValueConst -> Value
literal _ = error "literal: FIXME"

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
  , dInfo :: a
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
  | forall k. ('Input <: k) => IFOptional (NonNullableType k) (Maybe ValueConst)

data instance FieldInfo 'Output = forall k. ('Output <: k) => FieldInfo
  { fArguments :: [Definition (FieldInfo 'Input)]
  , fType :: Type k
  }

data Parser k m a = Parser
  { pType :: Type k
  , pParser :: ParserInput k -> m a
  } deriving (Functor)

type family ParserInput k where
  ParserInput 'Both = Value
  ParserInput 'Input = Value
  ParserInput 'Output = SelectionSet

-- | The constraint @(''Input' '<:' k)@ entails @('ParserInput' k ~ 'Value')@,
-- but GHC can’t figure that out on its own, so we have to be explicit to give
-- it a little help.
inputParserInput :: forall k. 'Input <: k => ParserInput k :~: Value
inputParserInput = case subKind @'Input @k of { KRefl -> Refl; KBoth -> Refl }

pInputParser :: forall k m a. 'Input <: k => Parser k m a -> Value -> m a
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
  -- `Compose ((,) [Definition InputFieldInfo]) (ReaderT (HashMap Name Value) m) a`,
  -- but working with that type kind of sucks.
  { ifDefinitions :: [Definition (FieldInfo k)]
  , ifParser :: HashMap Name (FieldInput k) -> m a
  } deriving (Functor)

type family FieldInput k = r | r -> k where
  FieldInput 'Input = Value
  FieldInput 'Output = Field

instance Applicative m => Applicative (FieldsParser k m) where
  pure v = FieldsParser [] (const $ pure v)
  a <*> b = FieldsParser
    (ifDefinitions a <> ifDefinitions b)
    (liftA2 (<*>) (ifParser a) (ifParser b))

data ScalarRepresentation a where
  SRBoolean :: ScalarRepresentation Bool
  SRInt :: ScalarRepresentation Int32
  SRFloat :: ScalarRepresentation Double
  SRString :: ScalarRepresentation Text

scalar
  :: MonadError Text m
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
  , pParser = \v -> case representation of
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
        VString (StringValue a) -> pure a
        _ -> typeMismatch name "a string" v
  }

boolean :: MonadError Text m => Parser 'Both m Bool
boolean = scalar "Boolean" Nothing SRBoolean

int :: MonadError Text m => Parser 'Both m Int32
int = scalar "Int" Nothing SRInt

float :: MonadError Text m => Parser 'Both m Double
float = scalar "Float" Nothing SRFloat

string :: MonadError Text m => Parser 'Both m Text
string = scalar "String" Nothing SRString

enum
  :: MonadError Text m
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
  , pParser = \case
      VEnum (EnumValue value) -> case M.lookup value valuesMap of
        Just result -> pure result
        Nothing -> throwError $ "expected one of the values "
          <> englishList "or" (dquoteTxt . dName . fst <$> values) <> "for type "
          <> name <<> ", but found " <>> value
      other -> typeMismatch name "an enum value" other
  }
  where
    valuesMap = M.fromList $ over (traverse._1) dName $ toList values

nullable :: forall k m a. (Applicative m, 'Input <: k) => Parser k m a -> Parser k m (Maybe a)
nullable parser = gcastWith (inputParserInput @k) $ Parser
  { pType = case pType parser of
      NonNullable t -> Nullable t
      Nullable t -> Nullable t
  , pParser = \case
      -- FIXME: Handle variables!
      VNull -> pure Nothing
      other -> Just <$> pParser parser other
  }

list :: forall k m a. (MonadError Text m, 'Input <: k) => Parser k m a -> Parser k m [a]
list parser = gcastWith (inputParserInput @k) $ Parser
  { pType = NonNullable $ TList $ pType parser
  , pParser = \case
      VList (ListValueG values) -> traverse (pParser parser) values
      other -> throwError $ "expected a list, but found " <> describeValue other
  }

object
  :: MonadError Text m
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
  , pParser = \case
      VObject (ObjectValueG fields) -> do
        -- check for extraneous fields here, since the FieldsParser just
        -- handles parsing the fields it cares about
        for_ fields \(ObjectFieldG fieldName _) -> do
          unless (fieldName `S.member` fieldNames) $
            throwError $ name <<> " has no field named " <>> fieldName
        ifParser parser $! M.fromList (fieldToPair <$> fields)
      other -> typeMismatch name "an object" other
  }
  where
    fieldNames = S.fromList (dName <$> ifDefinitions parser)
    fieldToPair (ObjectFieldG fieldName value) = (fieldName, value)

field
  :: (MonadError Text m, 'Input <: k)
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
          Nullable typ -> IFOptional typ (Just VCNull)
      }]
    , ifParser = M.lookup name
        >>> (`onNothing` throwError ("missing required field " <>> name))
        >=> pInputParser parser
    }
  -- nullable fields just have an implicit default value of `null`
  Nullable _ -> fieldWithDefault name description VCNull parser

fieldWithDefault
  :: 'Input <: k
  => Name
  -> Maybe Description
  -> ValueConst -- ^ default value
  -> Parser k m a
  -> FieldsParser 'Input m a
fieldWithDefault name description defaultValue parser = FieldsParser
  { ifDefinitions = [Definition
    { dName = name
    , dDescription = description
    , dInfo = IFOptional (discardNullability $ pType parser) (Just defaultValue)
    }]
  , ifParser = pInputParser parser . fromMaybe (literal defaultValue) . M.lookup name
  }

-- | A nullable field with no default value. If the field is omitted, the provided parser
-- /will not be called/. This allows a field to distinguish an omitted field from a field supplied
-- with @null@ (which is permitted by the GraphQL specification). If you want a field that defaults
-- to @null@, combine 'field' with 'nullable', instead.
fieldOptional
  :: (Applicative m, 'Input <: k)
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
  , ifParser = traverse (pInputParser parser) . M.lookup name
  }

selectionSet
  :: MonadError Text m
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
  , pParser = \selectionSet -> do
      let fields = selectionSet & mapMaybe \case
            -- FIXME: handle fragments
            SelectionField field -> Just field
            _ -> Nothing
      -- check for extraneous fields here, since the FieldsParser just
      -- handles parsing the fields it cares about
      for_ fields \Field{ _fName = fieldName } -> do
        unless (fieldName `S.member` fieldNames) $
          throwError $ name <<> " has no field named " <>> fieldName
      ifParser parser $! M.fromListOn _fName fields
  }
  where
    fieldNames = S.fromList (dName <$> ifDefinitions parser)

type family SelectionResult k a b where
  SelectionResult 'Both   a _ = (Alias, a)
  SelectionResult 'Output a b = (Alias, a, b)

selection
  :: forall k m a b
   . (MonadError Text m, 'Output <: k)
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
  , ifParser = M.lookup name >>> traverse \selectionField -> do
      let Field{ _fName = fieldName, _fArguments = arguments } = selectionField
          alias = _fAlias selectionField & fromMaybe (Alias fieldName)

      for_ arguments \(Argument argumentName _) -> do
        unless (argumentName `S.member` argumentNames) $
          throwError $ name <<> " has no argument named " <>> argumentName
      parsedArguments <- ifParser argumentsParser $! M.fromList (argumentToPair <$> arguments)

      case subKind @'Output @k of
        KRefl -> do
          parsedBody <- pParser bodyParser $ _fSelectionSet selectionField
          pure (alias, parsedArguments, parsedBody)
        KBoth -> pure (alias, parsedArguments)
  }
  where
    argumentNames = S.fromList (dName <$> ifDefinitions argumentsParser)
    argumentToPair (Argument argumentName value) = (argumentName, value)


typeMismatch :: MonadError Text m => Name -> Text -> Value -> m a
typeMismatch name expected given = throwError $
  "expected " <> expected <> " for type " <> name <<> ", but found " <> describeValue given

describeValue :: Value -> Text
describeValue = \case
  VInt _ -> "an integer"
  VFloat _ -> "a float"
  VString _ -> "a string"
  VBoolean _ -> "a boolean"
  VNull -> "null"
  VEnum _ -> "an enum value"
  VList _ -> "a list"
  VObject _ -> "an object"

-- -------------------------------------------------------------------------------------------------

column :: MonadError Text m => PGColumnType -> Parser 'Both m (WithScalarType PGScalarValue)
column columnType = case columnType of
  PGColumnScalar scalarType -> WithScalarType scalarType <$> case scalarType of
    PGInteger -> PGValInteger <$> int
    PGBoolean -> PGValBoolean <$> boolean
    PGFloat   -> PGValDouble <$> float
    PGText    -> PGValText <$> string
    PGVarchar -> PGValVarchar <$> string
    _         -> PGValUnknown <$> scalar name Nothing SRString -- FIXME: is SRString right?
  PGColumnEnumReference (EnumReference _ enumValues) -> case nonEmpty (M.toList enumValues) of
    Just enumValuesList ->
      WithScalarType PGText <$> enum name Nothing (mkEnumValue <$> enumValuesList)
    Nothing -> error "empty enum values" -- FIXME
  where
    name = unNamedType $ mkColumnType columnType
    -- FIXME: unify these types, avoid unchecked conversion to Name
    mkEnumValue (RQL.EnumValue value, RQL.EnumValueInfo description) =
      ( Definition (Name value) (Description <$> description) EnumValueInfo
      , PGValText value )

-- -------------------------------------------------------------------------------------------------

type OpExp = OpExpG UnresolvedVal

mkBoolExpr
  :: MonadError Text m
  => Name
  -> Maybe Description
  -> FieldsParser 'Input m [a]
  -> Parser 'Input m (GBoolExp a)
mkBoolExpr name description fields =
  fix \recur -> BoolAnd <$> object name description do
    basicFields <- map BoolFld <$> fields
    specialFields <- catMaybes <$> sequenceA
      [ fieldOptional "_or" Nothing (BoolOr <$> list recur)
      , fieldOptional "_and" Nothing (BoolAnd <$> list recur)
      , fieldOptional "_not" Nothing (BoolNot <$> recur)
      ]
    pure (basicFields ++ specialFields)

comparisonExprs :: MonadError Text m => PGColumnType -> Parser 'Input m [OpExp]
comparisonExprs columnType = object name Nothing $ catMaybes <$> sequenceA
  [ fieldOptional "_cast" Nothing (ACast <$> castExpr columnType)
  , fieldOptional "_eq" Nothing (AEQ True . UVPG . AnnPGVal _ _ <$> column columnType)
  -- etc.
  ]
  where
    name = unNamedType (mkColumnType columnType & addTypeSuffix "_comparison_exp")

castExpr :: forall m. MonadError Text m => PGColumnType -> Parser 'Input m (CastExp UnresolvedVal)
castExpr sourceType = object name Nothing (M.fromList . catMaybes <$> traverse mkField targetTypes)
  where
    name = unNamedType (mkColumnType sourceType & addTypeSuffix "_cast_exp")
    targetTypes = case sourceType of
      PGColumnScalar PGGeometry -> [PGGeography]
      PGColumnScalar PGGeography -> [PGGeometry]
      _ -> []

    mkField :: PGScalarType -> FieldsParser 'Input m (Maybe (PGScalarType, [OpExp]))
    mkField targetType = fieldOptional fieldName Nothing $
      (targetType,) <$> comparisonExprs (PGColumnScalar targetType)
      where
        fieldName = unNamedType $ mkColumnType $ PGColumnScalar targetType












{-

data Kind = Output | Input

data TypeRep k where
  RScalar :: ScalarRep -> TypeRep k
  RNullable :: TypeRep k -> TypeRep k
  RList :: TypeRep k -> TypeRep k
  REnum :: Nat -> TypeRep k
  RObject :: FieldsRep FieldRep -> TypeRep 'Output
  RUnion :: [TypeRep 'Output] -> TypeRep 'Output
  RInputObject :: FieldsRep (TypeRep 'Input) -> TypeRep 'Input

data ScalarRep
  = RString
  | RBoolean
  | RInt
  | RFloat

data FieldsRep r
  = FREmpty
  | FRField r (FieldsRep r)
  | FRMap r

data FieldRep = RField (FieldsRep (TypeRep 'Input)) (TypeRep 'Output)

data Nullability = NonNullable | Nullable
type family PossiblyNullable n s where
  PossiblyNullable 'NonNullable s = s
  PossiblyNullable 'Nullable s = 'RNullable s

$(genSingletons [''ScalarRep, ''Nullability])

data Definition a = Definition
  { dName :: Name
  , dDescription :: Maybe Description
  , dInfo :: a
  }

data TypeInfo (r :: TypeRep k) where
  TIScalar :: Sing r -> TypeInfo ('RScalar r)
  TIEnum :: Vector n (EnumValue, Description) -> TypeInfo ('REnum n)
  TIObject :: FieldsDefinition fr -> TypeInfo ('RObject fr)
  TIInputObject :: InputFieldsDefinition fr -> TypeInfo ('RInputObject fr)

data Type r where
  TNamed :: Definition (TypeInfo r) -> Type r
  TList :: Type r -> Sing n -> Type (PossiblyNullable n ('RList r))

data InputFieldsDefinition fr where
  IFDEmpty :: InputFieldsDefinition 'FREmpty
  IFDField
    :: Definition (InputFieldType r)
    -> InputFieldsDefinition fr
    -> InputFieldsDefinition ('FRField r fr)

data InputFieldType r where
  IFTRequired :: Type r -> InputFieldType r
  IFTOptional :: Type r -> TypedValue r -> InputFieldType ('RNullable r)

data FieldsDefinition fr

data TypedValue s where
  TVString :: Text -> TypedValue ('RScalar 'RString)
  TVBoolean :: Bool -> TypedValue ('RScalar 'RBoolean)
  TVInt :: Int32 -> TypedValue ('RScalar 'RInt)
  TVFloat :: Double -> TypedValue ('RScalar 'RFloat)
  TVNullable :: Maybe (TypedValue s) -> TypedValue ('RNullable s)
  TVEnum :: Finite n -> TypedValue ('REnum n)
  -- TVInputObject ::

validateScalar
  :: forall m s. MonadError Text m
  => Definition (TypeInfo ('RScalar s)) -> Value -> m (TypedValue ('RScalar s))
validateScalar Definition{ dName, dInfo = TIScalar shape } v = case shape of
  SRString -> case v of
    VString (StringValue s) -> pure $ TVString s
    _ -> representationMismatch "string"
  SRBoolean -> case v of
    VBoolean b -> pure $ TVBoolean b
    _ -> representationMismatch "boolean"
  SRInt -> case v of
    VInt i -> pure $ TVInt i
    _ -> representationMismatch "integer"
  SRFloat -> case v of
    VInt i -> pure $ TVFloat (fromIntegral i)
    VFloat f -> pure $ TVFloat f
    _ -> representationMismatch "number"
  where
    representationMismatch :: Text -> m a
    representationMismatch expected =
      throwError $ "expected a " <> expected <> " for value of type " <> unName dName


-- validateObject :: Type ('SObject fs) -> SelectionSet -> Either Text (TypedValue ('SObject fs))

data Parser r a = Parser
  { pType :: Definition (TypeInfo r)
  , pParse :: TypedValue r -> a
  } deriving (Functor)

int :: Parser ('RScalar 'RInt) Int32
int = Parser
  { pType = Definition
    { dName = Name "Int"
    , dDescription = Nothing
    , dInfo = TIScalar SRInt
    }
  , pParse = \(TVInt i) -> i
  }

-}
