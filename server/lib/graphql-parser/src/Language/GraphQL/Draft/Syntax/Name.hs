{-# LANGUAGE DeriveDataTypeable #-}
{-# HLINT ignore "Use onNothing" #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Internal functionality for Name values.
--
-- This module is necessary to avoid exposing `unName` and friends to the outside world.
module Language.GraphQL.Draft.Syntax.Name
  ( Name (..),
    NameSuffix (..),
    mkName,
    unsafeMkName,
    parseName,
    mkNameSuffix,
    parseSuffix,
    isValidName,
    addSuffixes,
    convertNameToSuffix,
    litName,
    litSuffix,
    litGQLIdentifier,
  )
where

-------------------------------------------------------------------------------

import Autodocodec (HasCodec (codec), bimapCodec)
import Control.DeepSeq (NFData)
import Data.Aeson qualified as J
import Data.Char qualified as C
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text qualified as T
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift)
import Language.Haskell.TH.Syntax.Compat (SpliceQ, examineSplice, liftSplice)
import Prettyprinter (Pretty (..))
import Prelude

-------------------------------------------------------------------------------

-- Defined here and re-exported in the public module to avoid exporting `unName`.`
newtype Name = Name {unName :: Text}
  deriving stock (Data, Eq, Lift, Ord, Show)
  deriving newtype (Semigroup, Hashable, NFData, Pretty, J.ToJSONKey, J.ToJSON)

instance HasCodec Name where
  codec = bimapCodec (\text -> maybe (Left $ T.unpack text <> " is not valid GraphQL name") Right $ mkName text) unName codec

-- | @NameSuffix@ is essentially a GQL identifier that can be used as Suffix
--  It is slightely different from @Name@ as it relaxes the criteria that a
--  @Name@ cannot start with a digit.
newtype NameSuffix = Suffix {unNameSuffix :: Text}
  deriving stock (Eq, Lift, Ord, Show)
  deriving newtype (Semigroup, Hashable, NFData, Pretty, J.ToJSONKey, J.ToJSON)

parseName :: (MonadFail m) => Text -> m Name
parseName text = maybe (fail errorMessage) pure $ mkName text
  where
    errorMessage = T.unpack text <> " is not valid GraphQL name"

-- | @matchFirst@ verifies if the starting character is according to the
--  graphql spec (refer https://spec.graphql.org/October2021/#NameStart).
matchFirst :: Char -> Bool
matchFirst c = c == '_' || C.isAsciiUpper c || C.isAsciiLower c

-- | @matchBody@ verifies if the continuing character is according to the
--  graphql spec (refer https://spec.graphql.org/October2021/#NameContinue).
matchBody :: Char -> Bool
matchBody c = c == '_' || C.isAsciiUpper c || C.isAsciiLower c || C.isDigit c

-- | @isValidName@ verifies if a text is a valid @Name@ as per the graphql
--  spec (refer https://spec.graphql.org/October2021/#Name)
isValidName :: Text -> Bool
isValidName text =
  case T.uncons text of
    Nothing -> False
    Just (first, body) ->
      matchFirst first && T.all matchBody body

mkName :: Text -> Maybe Name
mkName text =
  if isValidName text
    then Just (Name text)
    else Nothing

mkNameSuffix :: Text -> Maybe NameSuffix
mkNameSuffix text =
  if T.all matchBody text
    then Just (Suffix text)
    else Nothing

addSuffixes :: Name -> [NameSuffix] -> Name
addSuffixes prefix [] = prefix
addSuffixes (Name prefix) suffs = Name $ T.concat (prefix : suffsT)
  where
    suffsT = map unNameSuffix suffs

-- | All @Name@s are @Suffix@, so this function won't fail
convertNameToSuffix :: Name -> NameSuffix
convertNameToSuffix = coerce

unsafeMkName :: Text -> Name
unsafeMkName = Name

parseSuffix :: (MonadFail m) => Text -> m NameSuffix
parseSuffix text = maybe (fail errorMessage) pure $ mkNameSuffix text
  where
    errorMessage = T.unpack text <> " is not valid GraphQL suffix"

-- | Construct a 'Name' value at compile-time.
litName :: Text -> SpliceQ Name
litName txt = liftSplice do
  name <- parseName txt
  examineSplice [||name||]

-- | Construct a 'NameSuffix' value at compile-time.
litSuffix :: Text -> SpliceQ NameSuffix
litSuffix txt = liftSplice do
  name <- parseSuffix txt
  examineSplice [||name||]

-- | Construct prefix-suffix tuple at compile-time from a list.
litGQLIdentifier :: [Text] -> SpliceQ (Name, [NameSuffix])
litGQLIdentifier [] = liftSplice $ fail "GQL identifier cannot be empty"
litGQLIdentifier (x : xs) = liftSplice do
  pref <- parseName x
  suffs <- traverse parseSuffix xs
  examineSplice [||(pref, suffs)||]

instance J.FromJSON Name where
  parseJSON = J.withText "Name" parseName

instance J.FromJSONKey Name where
  fromJSONKey = J.FromJSONKeyTextParser parseName
