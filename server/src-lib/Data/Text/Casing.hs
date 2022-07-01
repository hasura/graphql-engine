-- | This module is inspired by @casing@ package. Instead of @String@ this
--  package uses @Data.Text.Text@
--
-- - @PascalCase@ - no spacing between words, first letter in word is
--    uppercase, all others are lowercase.
-- - @camelCase@ - like @PascalCase@, but the very first letter is lowercase.
-- - @snake_Case@ - underscores delimit words, case is unrestricted.
module Data.Text.Casing
  ( -- * Types
    GQLNameIdentifier (..),

    -- * @Data.Text@ converters
    toCamelT,
    toPascalT,
    toSnakeT,

    -- * GQLName generators
    toCamelG,
    toPascalG,
    toSnakeG,

    -- * Shorthand functions for @Data.Text@
    snakeToCamel,
    snakeToPascal,

    -- * Parser for @Data.Text@
    fromSnake,

    -- * Transformers
    transformNameWith,
    transformGQLSuffixWith,

    -- * Helpers
    identifierToList,
    fromTuple,
    fromName,
  )
where

import Data.List (intersperse)
import Data.Text qualified as T
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as G

-- | An opaque type, representing a parsed identifier with prefix and suffixes.
data GQLNameIdentifier = Identifier
  { namePrefix :: G.Name,
    nameSuffixes :: [G.NameSuffix]
    -- Using Vectors instead of list may improve memory uses
  }
  deriving (Show)

instance (Semigroup GQLNameIdentifier) where
  (Identifier pref1 suffs1) <> (Identifier pref2 suffs2) = Identifier pref1 (suffs1 <> ((G.convertNameToSuffix pref2) : suffs2))

fromTuple :: (G.Name, [G.NameSuffix]) -> GQLNameIdentifier
fromTuple (pref, suffs) = Identifier pref suffs

fromName :: G.Name -> GQLNameIdentifier
fromName n = Identifier n []

-- | transforms a graphql name with a transforming function
--
-- Note: This will return the graphql name without transformation if the
-- transformed name is not a valid GraphQL identifier
transformNameWith :: (Text -> Text) -> G.Name -> G.Name
transformNameWith f name = fromMaybe name (G.mkName (f (G.unName name)))

-- | similar to @transformNameWith@ but transforms @NameSuffix@ instead of
--  @Name@
transformGQLSuffixWith :: (Text -> Text) -> G.NameSuffix -> G.NameSuffix
transformGQLSuffixWith f suffix = fromMaybe suffix (G.mkNameSuffix (f (G.unNameSuffix suffix)))

-- | converts identifiers to @Text@ (i.e. @unName@s and @unNameSuffix@s
--  identifiers)
identifierToList :: GQLNameIdentifier -> [Text]
identifierToList (Identifier pref suffs) = (G.unName pref) : (map G.unNameSuffix suffs)

-- | To @snake_case@ for @Data.Text@
--
-- >>> toSnakeT ["my","random","text","list"]
-- "my_random_text_list"
toSnakeT :: [Text] -> Text
toSnakeT = T.concat . intersperse "_"

-- | To @PascalCase@ for @Data.Text@
--
-- >>> toPascalT ["my","random","text","list"]
-- "MyRandomTextList"
toPascalT :: [Text] -> Text
toPascalT = T.concat . map upperFirstChar

-- | To @camelCase@ for @Data.Text@
--
-- >>> toCamelT ["my","random","text","list"]
-- "myRandomTextList"
toCamelT :: [Text] -> Text
toCamelT ([]) = ""
toCamelT ((x : xs)) = (lowerFirstChar x) <> T.concat (map upperFirstChar xs)

-- | To @snake_case@ for @GQLNameIdentifier@
toSnakeG :: GQLNameIdentifier -> G.Name
toSnakeG (Identifier pref suff) = G.addSuffixes pref (map (transformGQLSuffixWith ("_" <>)) suff)

-- | To @PascalCase@ for @GQLNameIdentifier@
toPascalG :: GQLNameIdentifier -> G.Name
toPascalG (Identifier pref suff) = G.addSuffixes pref (map (transformGQLSuffixWith upperFirstChar) suff)

-- | To @camelCase@ for @GQLNameIdentifier@
toCamelG :: GQLNameIdentifier -> G.Name
toCamelG (Identifier pref []) = pref
toCamelG (Identifier x xs) = G.addSuffixes (transformNameWith lowerFirstChar x) (map (transformGQLSuffixWith upperFirstChar) xs)

-- @fromSnake@ is used in splitting the schema/table names separated by @_@
-- For global naming conventions:
-- We do not want to capture the underscore in the begining as a delimiter
-- and want to store it as it is. A user might have a schema that starts with
-- an underscore (we want to treat it as a part of the schema name rather
-- than a delimiter)
--

-- | Convert from @snake_cased@
--
-- >>> fromSnake "_hello_world_foo"
-- ["_hello","world","foo"]
fromSnake :: Text -> [Text]
fromSnake t = case T.splitOn "_" t of
  ("" : x : xs) -> ("_" <> x) : xs
  bs -> bs

-- | Directly convert to @PascalCase@ through 'fromSnake'
snakeToPascal :: Text -> Text
snakeToPascal = toPascalT . fromSnake

-- | Directly convert to @camelCase@ through 'fromSnake'
snakeToCamel :: Text -> Text
snakeToCamel = toCamelT . fromSnake

-- Internal helpers

-- | An internal helper function to lowercase the first character
lowerFirstChar :: Text -> Text
lowerFirstChar t = T.toLower (T.take 1 t) <> T.drop 1 t

-- | An internal helper function to uppercase the first character
upperFirstChar :: Text -> Text
upperFirstChar t = T.toUpper (T.take 1 t) <> T.drop 1 t
