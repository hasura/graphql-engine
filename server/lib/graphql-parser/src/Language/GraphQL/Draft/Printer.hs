{-# HLINT ignore "Use tshow" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.GraphQL.Draft.Printer
  ( Printer (..),
    executableDocument,
    graphQLType,
    renderExecutableDoc,
    schemaDocument,
    selectionSet,
    typeDefinitionP,
    value,
  )
where

-------------------------------------------------------------------------------

import Data.Aeson qualified as J
import Data.Bool (bool)
import Data.ByteString.Builder qualified as BS
import Data.ByteString.Builder.Scientific qualified as BSBS
import Data.Char (isControl)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (intersperse, sort)
import Data.Scientific (Scientific)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT hiding (singleton)
import Data.Text.Lazy.Builder qualified as LT (Builder)
import Data.Text.Lazy.Builder qualified as LTB
import Data.Text.Lazy.Builder.Int qualified as LTBI
import Data.Text.Lazy.Builder.Scientific qualified as LTBS
import Data.Text.Lazy.Encoding qualified as LTE
import Data.Void (Void, absurd)
import Language.GraphQL.Draft.Syntax
import Language.GraphQL.Draft.Syntax.Name qualified as Name
import Prettyprinter qualified as PP
import Text.Builder qualified as Text
import Prelude

-------------------------------------------------------------------------------

class (Monoid a, IsString a) => Printer a where
  textP :: Text -> a
  charP :: Char -> a
  intP :: Integer -> a
  doubleP :: Scientific -> a

  {-# MINIMAL textP, charP, intP, doubleP #-}

  nameP :: Name -> a
  nameP = textP . Name.unName

  nodeP :: (Print (frag var), Print var) => TypedOperationDefinition frag var -> a
  nodeP = node

  selectionSetP :: (Print (frag var), Print var) => SelectionSet frag var -> a
  selectionSetP = selectionSet

instance Printer BS.Builder where
  textP = LTE.encodeUtf8Builder . LT.fromStrict
  {-# INLINE textP #-}

  charP = BS.charUtf8
  {-# INLINE charP #-}

  intP = BS.integerDec
  {-# INLINE intP #-}

  doubleP = BSBS.scientificBuilder
  {-# INLINE doubleP #-}

instance Printer LT.Builder where
  textP = LTB.fromText
  {-# INLINE textP #-}

  charP = LTB.singleton
  {-# INLINE charP #-}

  intP = LTBI.decimal
  {-# INLINE intP #-}

  doubleP = LTBS.scientificBuilder
  {-# INLINE doubleP #-}

instance Printer (PP.Doc Text) where
  textP = PP.pretty
  {-# INLINE textP #-}

  charP = PP.pretty
  {-# INLINE charP #-}

  intP = PP.pretty
  {-# INLINE intP #-}

  -- NOTE: @prettyprinter@ constructs its 'Int', 'Float', etc. instances with
  -- 'unsafeViaShow', so it fine for us to use it here since 'Scientific'
  -- satisfies the requirement that the 'Show' instance must not have newlines.
  doubleP = PP.unsafeViaShow
  {-# INLINE doubleP #-}

  nameP = PP.pretty
  {-# INLINE nameP #-}

instance Printer Text.Builder where
  textP = Text.text
  {-# INLINE textP #-}

  charP = Text.char
  {-# INLINE charP #-}

  intP = Text.decimal
  {-# INLINE intP #-}

  doubleP = Text.string . show
  {-# INLINE doubleP #-}

instance Printer T.Text where
  textP = id
  charP = T.singleton
  intP = T.pack . show
  doubleP = T.pack . show

class Print a where
  printP :: (Printer b) => a -> b

instance Print Void where
  printP = absurd

instance Print Name where
  printP = nameP

-- Don't inline, to avoid the risk of unreasonably long code being generated
{-# NOINLINE builtInScalars #-}
builtInScalars :: [Name]
builtInScalars =
  [ Name.Name "Boolean",
    Name.Name "Float",
    Name.Name "ID",
    Name.Name "Int",
    Name.Name "String"
  ]

renderExecutableDoc :: ExecutableDocument Name -> Text
renderExecutableDoc = Text.run . executableDocument

-- | the pretty printer implementation
executableDocument :: (Print var, Printer a) => ExecutableDocument var -> a
executableDocument ed =
  mconcat $
    intersperse (charP '\n') $
      map executableDefinition $
        getExecutableDefinitions ed

executableDefinition :: (Print var, Printer a) => ExecutableDefinition var -> a
executableDefinition = \case
  ExecutableDefinitionOperation d -> operationDefinition d
  ExecutableDefinitionFragment d -> fragmentDefinition d

operationDefinition :: (Print (frag var), Print var, Printer a) => OperationDefinition frag var -> a
operationDefinition = \case
  OperationDefinitionUnTyped selSet -> selectionSetP selSet
  OperationDefinitionTyped op -> typedOperationDefinition op

typedOperationDefinition :: (Print (frag var), Print var, Printer a) => TypedOperationDefinition frag var -> a
typedOperationDefinition op =
  operationType (_todType op) <> charP ' ' <> nodeP op

operationType :: (Printer a) => OperationType -> a
operationType = \case
  OperationTypeQuery -> "query"
  OperationTypeMutation -> "mutation"
  OperationTypeSubscription -> "subscription"

-- TODO: add horizontal nesting
node :: (Print (frag var), Print var, Printer a) => TypedOperationDefinition frag var -> a
node (TypedOperationDefinition _ name vars dirs sels) =
  maybe mempty nameP name
    <> optempty variableDefinitions vars
    <> optempty directives dirs
    <> charP ' '
    <> selectionSetP sels

-- TODO: add horizontal nesting
selectionSet :: (Print (frag var), Print var, Printer a) => SelectionSet frag var -> a
selectionSet [] = mempty
selectionSet xs =
  "{ " <> mconcat (intersperse (charP ' ') (map selection xs)) <> " }"

selection :: (Print (frag var), Print var, Printer a) => Selection frag var -> a
selection = \case
  SelectionField fld -> field fld
  SelectionFragmentSpread fs -> printP fs
  SelectionInlineFragment ilf -> inlineFragment ilf

field :: (Print (frag var), Print var, Printer a) => Field frag var -> a
field (Field alias name args dirs selSets) =
  optAlias alias
    <> nameP name
    <> optempty arguments args
    <> optempty directives dirs
    <> charP ' '
    <> selectionSetP selSets

optAlias :: (Printer a) => Maybe Name -> a
optAlias = maybe mempty (\a -> nameP a <> textP ": ")

inlineFragment :: (Print (frag var), Print var, Printer a) => InlineFragment frag var -> a
inlineFragment (InlineFragment tc ds sels) =
  "... "
    <> maybe mempty ((textP "on " <>) . nameP) tc
    <> optempty directives ds
    <> selectionSetP sels

instance (Print var) => Print (FragmentSpread var) where
  printP (FragmentSpread name ds) =
    "..." <> nameP name <> optempty directives ds

instance Print (NoFragments var) where
  printP = \case {}

fragmentDefinition :: (Printer a) => FragmentDefinition -> a
fragmentDefinition (FragmentDefinition name tc dirs sels) =
  "fragment "
    <> nameP name
    <> " on "
    <> nameP tc
    <> optempty directives dirs
    <> selectionSetP sels

directives :: (Print var, Printer a) => [Directive var] -> a
directives = mconcat . intersperse (charP ' ') . map directive

directive :: (Print var, Printer a) => Directive var -> a
directive (Directive name args) =
  charP '@' <> nameP name <> optempty arguments args

arguments :: (Print var, Printer a) => HashMap Name (Value var) -> a
arguments xs = charP '(' <> objectFields xs <> charP ')'

variableDefinitions :: (Printer a) => [VariableDefinition] -> a
variableDefinitions vars =
  mconcat
    [ charP '(',
      mconcat vars',
      charP ')'
    ]
  where
    vars' = intersperse (charP ',') $ map variableDefinition vars

variableDefinition :: (Printer a) => VariableDefinition -> a
variableDefinition (VariableDefinition var ty defVal) =
  variableP var <> ": " <> graphQLType ty <> maybe mempty defaultValue defVal

defaultValue :: (Printer a) => Value Void -> a
defaultValue v = " = " <> value v

description :: (Printer a) => Maybe Description -> a
description Nothing = mempty
description (Just desc) = dispatchStringPrinter (unDescription desc) <> " \n"

-- | Type Reference
graphQLType :: (Printer a) => GType -> a
graphQLType (TypeNamed n x) = nameP x <> nonNull n
graphQLType (TypeList n x) = listType x <> nonNull n

listType :: (Printer a) => GType -> a
listType ty = charP '[' <> graphQLType ty <> charP ']'

nonNull :: (Printer a) => Nullability -> a
nonNull n = bool (charP '!') mempty $ unNullability n

-- | Primitives
variableP :: (Print a, Printer b) => a -> b
variableP v = charP '$' <> printP v

value :: (Print var, Printer a) => Value var -> a
value = \case
  VVariable v -> variableP v
  VInt i -> intP i
  VFloat d -> doubleP d
  VString s -> dispatchStringPrinter s
  VBoolean b -> fromBool b
  VNull -> "null"
  VList xs -> listValue xs
  VObject o -> objectValue o
  VEnum ev -> nameP $ unEnumValue ev

-- | Print a given text as a normal string or as a block string, depending on
-- its content.
dispatchStringPrinter :: (Printer a) => Text -> a
dispatchStringPrinter t =
  if printAsBlockString then blockStringValue t else stringValue t
  where
    printAsBlockString =
      hasNewlines && onlySourceCharacter && not (hasWhitespaceEnd || hasZeroIndentation || hasTripleQuotes)
    -- Condition 1: if there are no newlines, there's no point to print a text
    -- as a block string
    hasNewlines = "\n" `T.isInfixOf` t
    -- Condition 2: block strings only support GraphQL's SourceCharacters
    -- http://spec.graphql.org/June2018/#SourceCharacter
    onlySourceCharacter = T.all isSourceCharacter t
    -- Condition 3: if the text ends in a line containing only whitespace, we
    -- can't print it as a block string
    hasWhitespaceEnd = T.all isWhitespace $ T.takeWhileEnd (/= '\n') t
    -- Condition 4: if none of the remaining lines (i.e. not the first line)
    -- contains nonzero indentation, we can't print it as a block string
    hasZeroIndentation = any lineZeroIndentation $ tail $ T.lines t
      where
        lineZeroIndentation line = case T.uncons line of
          Nothing -> False -- empty lines don't count
          Just (firstChar, _) -> not (isWhitespace firstChar)
    -- Condition 5: although """ is printable in block strings as \""", this
    -- isn't currently implemented
    hasTripleQuotes = "\"\"\"" `T.isInfixOf` t

    isWhitespace :: Char -> Bool
    isWhitespace c = c == ' ' || c == '\t'
    isSourceCharacter :: Char -> Bool
    isSourceCharacter = not . isControl

-- | We use Aeson to decode string values, and therefore use Aeson to encode them back.
stringValue :: (Printer a) => Text -> a
stringValue s = textP $ LT.toStrict $ LTE.decodeUtf8 $ J.encode s

blockStringValue :: (Printer a) => Text -> a
blockStringValue t = textP "\"\"\"\n" <> textP t <> textP "\n\"\"\""

listValue :: (Print var, Printer a) => [Value var] -> a
listValue xs = mconcat [charP '[', li, charP ']']
  where
    li = mconcat $ intersperse (charP ',') $ map value xs

objectValue :: (Print var, Printer a) => HashMap Name (Value var) -> a
objectValue o = charP '{' <> objectFields o <> charP '}'

objectFields :: (Print var, Printer a) => HashMap Name (Value var) -> a
objectFields o = mconcat $ intersperse (charP ',') $ map objectField $ HashMap.toList o
  where
    objectField (name, val) = nameP name <> ": " <> value val

fromBool :: (Printer a) => Bool -> a
fromBool True = "true"
fromBool False = "false"

optempty :: (Foldable f, Monoid b) => (f a -> b) -> f a -> b
optempty f xs
  | null xs = mempty
  | otherwise = f xs

schemaDefinition ::
  forall a.
  (Printer a) =>
  SchemaDefinition ->
  a
schemaDefinition (SchemaDefinition dirs rootOpDefs) =
  "schema "
    <> maybe mempty (optempty directives) dirs
    <> " { "
    <> mconcat (intersperse (charP ' ') (map rootOperationTypeDefinition rootOpDefs))
    <> " }"

rootOperationTypeDefinition :: (Printer a) => RootOperationTypeDefinition -> a
rootOperationTypeDefinition (RootOperationTypeDefinition opType rootName) =
  operationType opType <> ": " <> nameP rootName

typeSystemDefinition :: (Printer a) => TypeSystemDefinition -> a
typeSystemDefinition (TypeSystemDefinitionSchema schemaDefn) = schemaDefinition schemaDefn
typeSystemDefinition (TypeSystemDefinitionType typeDefn) = typeDefinitionP typeDefn

schemaDocument :: (Printer a) => SchemaDocument -> a
schemaDocument (SchemaDocument typeDefns) =
  mconcat $ intersperse (textP "\n\n") $ map typeSystemDefinition $ sort $ filter isNotBuiltInScalar typeDefns
  where
    -- According to https://spec.graphql.org/June2018/#sec-Scalars:
    --   > When representing a GraphQL schema using the type system definition language, the built‐in scalar types should
    --   > be omitted for brevity.
    isNotBuiltInScalar
      ( TypeSystemDefinitionType
          (TypeDefinitionScalar (ScalarTypeDefinition _ name _))
        ) = name `notElem` builtInScalars
    isNotBuiltInScalar _ = True

typeDefinitionP :: (Printer a) => TypeDefinition () InputValueDefinition -> a
typeDefinitionP (TypeDefinitionScalar scalarDefn) = scalarTypeDefinition scalarDefn
typeDefinitionP (TypeDefinitionObject objDefn) = objectTypeDefinition objDefn
typeDefinitionP (TypeDefinitionInterface interfaceDefn) = interfaceTypeDefinition interfaceDefn
typeDefinitionP (TypeDefinitionUnion unionDefn) = unionTypeDefinition unionDefn
typeDefinitionP (TypeDefinitionEnum enumDefn) = enumTypeDefinition enumDefn
typeDefinitionP (TypeDefinitionInputObject inpObjDefn) = inputObjectTypeDefinition inpObjDefn

scalarTypeDefinition :: (Printer a) => ScalarTypeDefinition -> a
scalarTypeDefinition (ScalarTypeDefinition desc name dirs) =
  description desc
    <> "scalar "
    <> nameP name
    <> if null dirs
      then mempty
      else charP ' ' <> optempty directives dirs

inputValueDefinition :: (Printer a) => InputValueDefinition -> a
inputValueDefinition (InputValueDefinition desc name gType defVal dirs) =
  description desc
    <> nameP name
    <> textP ": "
    <> graphQLType gType
    <> maybe mempty defaultValue defVal
    <> if null dirs
      then mempty
      else charP ' ' <> optempty directives dirs

fieldDefinition :: (Printer a) => FieldDefinition InputValueDefinition -> a
fieldDefinition (FieldDefinition desc name args gType dirs) =
  description desc
    <> nameP name
    <> case args of
      [] -> mempty
      _ ->
        charP '('
          <> mconcat (intersperse (textP ", ") $ map inputValueDefinition args)
          <> charP ')'
    <> textP ": "
    <> graphQLType gType
    <> optempty directives dirs

objectTypeDefinition :: (Printer a) => ObjectTypeDefinition InputValueDefinition -> a
objectTypeDefinition (ObjectTypeDefinition desc name ifaces dirs fieldDefinitions) =
  description desc
    <> "type "
    <> nameP name
    <> optempty directives dirs
    <> case ifaces of
      [] -> mempty
      _ -> " implements " <> mconcat (intersperse (textP " & ") $ map nameP ifaces)
    <> " { "
    <> ( mconcat
           . intersperse (textP "\n  ")
           . map fieldDefinition
           . sort
           $ fieldDefinitions
       )
    <> "\n"
    <> "}"

interfaceTypeDefinition :: (Printer a) => InterfaceTypeDefinition () InputValueDefinition -> a
interfaceTypeDefinition (InterfaceTypeDefinition desc name dirs fieldDefinitions _possibleTypes) =
  -- `possibleTypes` are not included with an interface definition in a GraphQL IDL
  description desc
    <> "interface "
    <> nameP name
    <> charP ' '
    <> optempty directives dirs
    <> " { "
    <> mconcat
      ( intersperse (textP "\n  ")
          . map fieldDefinition
          . sort
          $ fieldDefinitions
      )
    <> "\n"
    <> "}"

unionTypeDefinition :: (Printer a) => UnionTypeDefinition -> a
unionTypeDefinition (UnionTypeDefinition desc name dirs members) =
  description desc
    <> "union "
    <> nameP name
    <> charP ' '
    <> optempty directives dirs
    <> textP " = "
    <> mconcat (intersperse (textP " | ") $ map nameP $ sort members)

enumValueDefinition :: (Printer a) => EnumValueDefinition -> a
enumValueDefinition (EnumValueDefinition desc name dirs) =
  description desc
    <> nameP (unEnumValue name)
    <> charP ' '
    <> optempty directives dirs

enumTypeDefinition :: (Printer a) => EnumTypeDefinition -> a
enumTypeDefinition (EnumTypeDefinition desc name dirs enumValDefns) =
  description desc
    <> "enum "
    <> nameP name
    <> optempty directives dirs
    <> " {"
    <> mconcat
      ( intersperse (textP "\n  ")
          . map enumValueDefinition
          . sort
          $ enumValDefns
      )
    <> "\n"
    <> "}"

inputObjectTypeDefinition :: (Printer a) => InputObjectTypeDefinition InputValueDefinition -> a
inputObjectTypeDefinition (InputObjectTypeDefinition desc name dirs valDefns) =
  description desc
    <> "input "
    <> nameP name
    <> optempty directives dirs
    <> " {"
    <> mconcat
      ( intersperse (textP "\n  ")
          . map inputValueDefinition
          . sort
          $ valDefns
      )
    <> "\n"
    <> "}"
