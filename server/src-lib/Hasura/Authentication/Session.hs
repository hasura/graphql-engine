module Hasura.Authentication.Session
  ( -- * Known headers
    adminSecretHeader,
    deprecatedAccessKeyHeader,
    useBackendOnlyPermissionsHeader,
    userIdHeader,
    userRoleHeader,

    -- * A single session variable
    SessionVariable,
    SessionVariableValue,
    ToSessionVariable (..),
    unsafeMkSessionVariable,
    parseSessionVariable,
    sessionVariableToHeader,
    sessionVariableToGraphQLName,

    -- * Multiple session variables
    SessionVariables,
    singleSessionVariable,
    sessionVariablesFromMap,
    sessionVariablesWith,
    sessionVariablesWithout,
    filterSessionVariables,
    sessionVariablesToHeaders,
    mkSessionVariablesHeaders,
    getSessionVariableValue,
    getSessionVariablesSet,
    getSessionVariables,

    -- * Miscellaneous
    maybeRoleFromSessionVariables,
    mkClientHeadersForward,
  )
where

import Data.Aeson
import Data.Aeson.Key qualified as K
import Data.Aeson.Types (Parser, toJSONKeyText)
import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Authentication.Header (filterHeaders)
import Hasura.Authentication.Headers (commonClientHeadersIgnored)
import Hasura.Authentication.Role (RoleName, mkRoleName)
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Types qualified as HTTP

adminSecretHeader :: SessionVariable
adminSecretHeader = SessionVariable "x-hasura-admin-secret"

userIdHeader :: SessionVariable
userIdHeader = SessionVariable "x-hasura-user-id"

userRoleHeader :: SessionVariable
userRoleHeader = SessionVariable "x-hasura-role"

useBackendOnlyPermissionsHeader :: SessionVariable
useBackendOnlyPermissionsHeader = SessionVariable "x-hasura-use-backend-only-permissions"

deprecatedAccessKeyHeader :: SessionVariable
deprecatedAccessKeyHeader = SessionVariable "x-hasura-access-key"

newtype SessionVariable = SessionVariable {unSessionVariable :: CI.CI Text}
  deriving (Show, Eq, Hashable, Data, NFData, Ord)

instance ToJSON SessionVariable where
  toJSON = toJSON . CI.original . unSessionVariable

instance ToJSONKey SessionVariable where
  toJSONKey = toJSONKeyText fromSessionVariable

instance ToTxt SessionVariable where
  toTxt = fromSessionVariable

type SessionVariableValue = Text

sessionVariablePrefix :: (IsString a) => a
sessionVariablePrefix = "x-hasura-"

parseSessionVariable :: Text -> Parser SessionVariable
parseSessionVariable name = mkSessionVariable name `onNothing` fail (show name <> " is not a Hasura session variable")

instance FromJSON SessionVariable where
  parseJSON = withText "String" parseSessionVariable

instance FromJSONKey SessionVariable where
  fromJSONKey = FromJSONKeyTextParser parseSessionVariable

class ToSessionVariable a where
  -- | Tests the input to find out whether it is a valid session variable.
  isSessionVariable :: a -> Bool

  -- | Makes a session variable from the input. Returns `Nothing` on an invalid input.
  mkSessionVariable :: a -> Maybe SessionVariable

  -- | Converts back to the input (lossily).
  fromSessionVariable :: SessionVariable -> a

instance ToSessionVariable Text where
  {-# INLINE isSessionVariable #-} -- hope any redundant conversions vis a vis SessionVariable are eliminated
  isSessionVariable = T.isPrefixOf sessionVariablePrefix . T.toCaseFold
  mkSessionVariable name = mkSessionVariable $ CI.mk name
  fromSessionVariable (SessionVariable var) = CI.foldedCase var

instance ToSessionVariable (CI.CI Text) where
  {-# INLINE isSessionVariable #-} -- hope any redundant conversions vis a vis SessionVariable are eliminated
  isSessionVariable = T.isPrefixOf sessionVariablePrefix . CI.foldedCase
  mkSessionVariable name =
    if isSessionVariable name
      then Just $ SessionVariable name
      else Nothing
  fromSessionVariable (SessionVariable var) = var

instance ToSessionVariable K.Key where
  isSessionVariable = isSessionVariable . K.toText
  mkSessionVariable = mkSessionVariable . K.toText
  fromSessionVariable = K.fromText . fromSessionVariable

instance ToSessionVariable HTTP.HeaderName where
  {-# INLINE isSessionVariable #-} -- hope any redundant conversions vis a vis SessionVariable are eliminated
  isSessionVariable = BS.isPrefixOf sessionVariablePrefix . CI.foldedCase
  mkSessionVariable = mkSessionVariable . CI.map bsToTxt
  fromSessionVariable (SessionVariable var) = CI.map txtToBs var

-- | Unsafely makes a session variable. Errors if the input is invalid.
unsafeMkSessionVariable :: (ToSessionVariable a) => a -> SessionVariable
unsafeMkSessionVariable = Maybe.fromJust . mkSessionVariable

sessionVariableToHeader :: SessionVariable -> SessionVariableValue -> HTTP.Header
sessionVariableToHeader variable value = (fromSessionVariable variable, txtToBs value)

newtype SessionVariables = SessionVariables {unSessionVariables :: HashMap.HashMap SessionVariable SessionVariableValue}
  deriving (Show, Eq, Hashable, Semigroup, Monoid)

instance ToJSON SessionVariables where
  toJSON (SessionVariables variables) =
    toJSON $ mapKeys (fromSessionVariable @K.Key) variables

-- Note: this discards anything that isn't a valid session variable.
instance FromJSON SessionVariables where
  parseJSON v = sessionVariablesFromMap <$> parseJSON v

singleSessionVariable :: SessionVariable -> SessionVariableValue -> SessionVariables
singleSessionVariable name value = SessionVariables $ HashMap.singleton name value

sessionVariablesFromMap :: HashMap Text Text -> SessionVariables
sessionVariablesFromMap = SessionVariables . keysToSessionVariables
  where
    keysToSessionVariables = HashMap.fromList . HashMap.foldrWithKey folder []
    folder k v xs =
      case mkSessionVariable k of
        Just k' -> (k', v) : xs
        Nothing -> xs

sessionVariablesWith :: SessionVariable -> SessionVariableValue -> SessionVariables -> SessionVariables
sessionVariablesWith name value = SessionVariables . HashMap.insert name value . unSessionVariables

sessionVariablesWithout :: SessionVariable -> SessionVariables -> SessionVariables
sessionVariablesWithout name = SessionVariables . HashMap.delete name . unSessionVariables

-- | Converts a `SessionVariable` value to a GraphQL name.
-- This will fail if the session variable contains characters that are not valid
-- for a graphql names. It is the caller's responsibility to decide what to do
-- in such a case.
sessionVariableToGraphQLName :: SessionVariable -> Maybe G.Name
sessionVariableToGraphQLName = G.mkName . T.replace "-" "_" . fromSessionVariable

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
    . filter (isSessionVariable . fst) -- Only x-hasura-* headers
    . map (CI.map bsToTxt *** bsToTxt)

---- Something like this a little faster, but I expect some test failures
--   . map (lowerToTxt *** bsToTxt)
--   where
--     -- NOTE: this throws away the original, vs 'CI.map bsToTxt'
--     lowerToTxt = CI.unsafeMk . bsToTxt . CI.foldedCase

sessionVariablesToHeaders :: SessionVariables -> [HTTP.Header]
sessionVariablesToHeaders =
  map (uncurry sessionVariableToHeader)
    . HashMap.toList
    . unSessionVariables

getSessionVariables :: SessionVariables -> [Text]
getSessionVariables = map fromSessionVariable . HashMap.keys . unSessionVariables

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
