module Hasura.Authentication.Session
  ( SessionVariable,
    SessionVariables (..),
    SessionVariableValue,
    parseSessionVariable,
    sessionVariableToText,
    mkSessionVariable,
    mkSessionVariablesText,
    isSessionVariable,
    filterSessionVariables,
    sessionVariableToGraphQLName,
    sessionVariablesToHeaders,
    mkSessionVariablesHeaders,
    getSessionVariableValue,
    getSessionVariablesSet,
    getSessionVariables,
    maybeRoleFromSessionVariables,
    mkClientHeadersForward,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser, toJSONKeyText)
import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Authentication.Header (filterHeaders)
import Hasura.Authentication.Headers (commonClientHeadersIgnored, userRoleHeader)
import Hasura.Authentication.Role (RoleName, mkRoleName)
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Types qualified as HTTP

newtype SessionVariable = SessionVariable {unSessionVariable :: CI.CI Text}
  deriving (Show, Eq, Hashable, IsString, Data, NFData, Ord)

instance ToJSON SessionVariable where
  toJSON = toJSON . CI.original . unSessionVariable

instance ToJSONKey SessionVariable where
  toJSONKey = toJSONKeyText sessionVariableToText

instance ToTxt SessionVariable where
  toTxt = sessionVariableToText

type SessionVariableValue = Text

sessionVariablePrefix :: Text
sessionVariablePrefix = "x-hasura-"

isSessionVariable :: Text -> Bool
{-# INLINE isSessionVariable #-} -- hope any redundant conversions vis a vis SessionVariable are eliminated
isSessionVariable = T.isPrefixOf sessionVariablePrefix . T.toCaseFold

-- | A more efficient form of 'isSessionVariable', where applicable
isSessionVariableCI :: CI.CI Text -> Bool
{-# INLINE isSessionVariableCI #-}
isSessionVariableCI = T.isPrefixOf sessionVariablePrefix . CI.foldedCase

parseSessionVariable :: Text -> Parser SessionVariable
parseSessionVariable t =
  -- for performance we avoid isSessionVariable, doing just one case conversion
  let sessionVar_dirty = mkSessionVariable t
   in if sessionVariablePrefix `T.isPrefixOf` CI.foldedCase (unSessionVariable sessionVar_dirty)
        then pure sessionVar_dirty
        else fail $ show t <> " is not a Hasura session variable"

instance FromJSON SessionVariable where
  parseJSON = withText "String" parseSessionVariable

instance FromJSONKey SessionVariable where
  fromJSONKey = FromJSONKeyTextParser parseSessionVariable

-- | in normalized, lower-case form
sessionVariableToText :: SessionVariable -> Text
sessionVariableToText = CI.foldedCase . unSessionVariable

mkSessionVariable :: Text -> SessionVariable
mkSessionVariable = SessionVariable . CI.mk

newtype SessionVariables = SessionVariables {unSessionVariables :: HashMap.HashMap SessionVariable SessionVariableValue}
  deriving (Show, Eq, Hashable, Semigroup, Monoid)

instance ToJSON SessionVariables where
  toJSON (SessionVariables varMap) =
    toJSON $ mapKeys sessionVariableToText varMap

instance FromJSON SessionVariables where
  parseJSON v = mkSessionVariablesText <$> parseJSON v

mkSessionVariablesText :: HashMap.HashMap Text Text -> SessionVariables
mkSessionVariablesText = SessionVariables . mapKeys mkSessionVariable

-- | Converts a `SessionVariable` value to a GraphQL name.
-- This will fail if the session variable contains characters that are not valid
-- for a graphql names. It is the caller's responsibility to decide what to do
-- in such a case.
sessionVariableToGraphQLName :: SessionVariable -> Maybe G.Name
sessionVariableToGraphQLName = G.mkName . T.replace "-" "_" . sessionVariableToText

filterSessionVariables ::
  (SessionVariable -> SessionVariableValue -> Bool) ->
  SessionVariables ->
  SessionVariables
filterSessionVariables f = SessionVariables . HashMap.filterWithKey f . unSessionVariables

mkSessionVariablesHeaders :: [HTTP.Header] -> SessionVariables
mkSessionVariablesHeaders =
  SessionVariables
    . HashMap.fromList
    . map (first SessionVariable)
    . filter (isSessionVariableCI . fst) -- Only x-hasura-* headers
    . map (CI.map bsToTxt *** bsToTxt)

---- Something like this a little faster, but I expect some test failures
--   . map (lowerToTxt *** bsToTxt)
--   where
--     -- NOTE: this throws away the original, vs 'CI.map bsToTxt'
--     lowerToTxt = CI.unsafeMk . bsToTxt . CI.foldedCase

sessionVariablesToHeaders :: SessionVariables -> [HTTP.Header]
sessionVariablesToHeaders =
  map ((CI.map txtToBs . unSessionVariable) *** txtToBs)
    . HashMap.toList
    . unSessionVariables

getSessionVariables :: SessionVariables -> [Text]
getSessionVariables = map sessionVariableToText . HashMap.keys . unSessionVariables

getSessionVariablesSet :: SessionVariables -> HashSet SessionVariable
getSessionVariablesSet = HashMap.keysSet . unSessionVariables

getSessionVariableValue :: SessionVariable -> SessionVariables -> Maybe SessionVariableValue
getSessionVariableValue k = HashMap.lookup k . unSessionVariables

maybeRoleFromSessionVariables :: SessionVariables -> Maybe RoleName
maybeRoleFromSessionVariables sessionVariables =
  -- returns Nothing if x-hasura-role is an empty string
  getSessionVariableValue userRoleHeader sessionVariables >>= mkRoleName

mkClientHeadersForward :: [HTTP.Header] -> [HTTP.Header]
mkClientHeadersForward reqHeaders =
  xForwardedHeaders <> (filterVars . filterRequestHeaders) reqHeaders
  where
    filterRequestHeaders = filterHeaders $ HashSet.fromList commonClientHeadersIgnored
    filterVars = filter (\(k, _) -> not $ isSessionVariable $ bsToTxt $ CI.original k)
    xForwardedHeaders = flip mapMaybe reqHeaders $ \(hdrName, hdrValue) ->
      case hdrName of
        "Host" -> Just ("X-Forwarded-Host", hdrValue)
        "User-Agent" -> Just ("X-Forwarded-User-Agent", hdrValue)
        "Origin" -> Just ("X-Forwarded-Origin", hdrValue)
        _ -> Nothing
