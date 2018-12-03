module Hasura.RQL.Types.Permission
       ( RoleName(..)
       , UserId(..)

       , UserVars
       , mkUserVars
       , isUserVar
       , getVarNames
       , getVarVal
       , roleFromVars

       , UserInfo
       , userRole
       , userVars
       , mkUserInfo
       , userInfoToList
       , adminUserInfo
       , adminRole
       , isAdmin
       , PermType(..)
       , permTypeToCode
       , PermId(..)
       ) where

import           Hasura.Prelude
import           Hasura.Server.Utils        (accessKeyHeader, userRoleHeader)
import           Hasura.SQL.Types

import qualified Database.PG.Query          as Q

import           Data.Aeson
import           Data.Hashable
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.HashMap.Strict        as Map
import qualified Data.Text                  as T
import qualified PostgreSQL.Binary.Decoding as PD

newtype RoleName
  = RoleName {getRoleTxt :: T.Text}
  deriving ( Show, Eq, Hashable, FromJSONKey, ToJSONKey, FromJSON
           , ToJSON, Q.FromCol, Q.ToPrepArg, Lift)

instance DQuote RoleName where
  dquoteTxt (RoleName r) = r

adminRole :: RoleName
adminRole = RoleName "admin"

isAdmin :: RoleName -> Bool
isAdmin = (adminRole ==)

newtype UserId = UserId { getUserId :: Word64 }
  deriving (Show, Eq, FromJSON, ToJSON)

newtype UserVars
  = UserVars { unUserVars :: Map.HashMap T.Text T.Text }
  deriving (Show, Eq, FromJSON, ToJSON, Hashable)

isUserVar :: T.Text -> Bool
isUserVar = T.isPrefixOf "x-hasura-" . T.toLower

roleFromVars :: UserVars -> Maybe RoleName
roleFromVars =
  fmap RoleName . getVarVal userRoleHeader

getVarVal :: Text -> UserVars -> Maybe Text
getVarVal k =
  Map.lookup k . unUserVars

getVarNames :: UserVars -> [T.Text]
getVarNames =
  Map.keys . unUserVars

mkUserVars :: [(T.Text, T.Text)] -> UserVars
mkUserVars l =
  UserVars $ Map.fromList
  [ (T.toLower k, v)
  | (k, v) <- l, isUserVar k
  ]

data UserInfo
  = UserInfo
  { userRole :: !RoleName
  , userVars :: !UserVars
  } deriving (Show, Eq, Generic)

mkUserInfo :: RoleName -> UserVars -> UserInfo
mkUserInfo rn (UserVars v) =
  UserInfo rn $ UserVars $ Map.insert userRoleHeader (getRoleTxt rn) $
  Map.delete accessKeyHeader v

instance Hashable UserInfo

-- $(J.deriveToJSON (J.aesonDrop 4 J.camelCase){J.omitNothingFields=True}
--   ''UserInfo
--  )

userInfoToList :: UserInfo -> [(Text, Text)]
userInfoToList userInfo =
  let vars = Map.toList $ unUserVars . userVars $ userInfo
      rn = getRoleTxt . userRole $ userInfo
  in (userRoleHeader, rn) : vars

adminUserInfo :: UserInfo
adminUserInfo =
  mkUserInfo adminRole $ mkUserVars []

data PermType
  = PTInsert
  | PTSelect
  | PTUpdate
  | PTDelete
  deriving (Eq, Lift)

instance Q.FromCol PermType where
  fromCol bs = flip Q.fromColHelper bs $ PD.enum $ \case
    "insert" -> Just PTInsert
    "update" -> Just PTUpdate
    "select" -> Just PTSelect
    "delete" -> Just PTDelete
    _   -> Nothing

permTypeToCode :: PermType -> T.Text
permTypeToCode PTInsert = "insert"
permTypeToCode PTSelect = "select"
permTypeToCode PTUpdate = "update"
permTypeToCode PTDelete = "delete"

instance Hashable PermType where
  hashWithSalt salt a = hashWithSalt salt $ permTypeToCode a

instance Show PermType where
  show PTInsert = "insert"
  show PTSelect = "select"
  show PTUpdate = "update"
  show PTDelete = "delete"

instance FromJSON PermType where
  parseJSON (String "insert") = return PTInsert
  parseJSON (String "select") = return PTSelect
  parseJSON (String "update") = return PTUpdate
  parseJSON (String "delete") = return PTDelete
  parseJSON _ =
    fail "perm_type should be one of 'insert', 'select', 'update', 'delete'"

instance ToJSON PermType where
  toJSON = String . permTypeToCode

data PermId
  = PermId
  { pidTable :: !TableName
  , pidRole  :: !RoleName
  , pidType  :: !PermType
  } deriving (Eq)

instance Show PermId where
  show (PermId tn rn pType) =
    show $ mconcat
    [ getTableTxt tn
    , "."
    , getRoleTxt rn
    , "."
    , T.pack $ show pType
    ]
