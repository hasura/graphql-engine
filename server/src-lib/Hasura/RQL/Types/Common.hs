{-# LANGUAGE PatternSynonyms #-}
module Hasura.RQL.Types.Common
       ( PGColInfo(..)
       , PGColInfo(JSONColInfo)
       , RelName(..)
       , RelType(..)
       , relTypeToTxt
       , RelInfo(..)

       , FieldName(..)
       , fromPGCol
       , fromRel

       , TQueryName(..)
       , TemplateParam(..)

       , ToAesonPairs(..)
       , WithTable(..)

       , mkPGTyMaps
       , PGTyInfoMaps
       , PGColOidInfo(..)
       , PGTyInfo(..)
       , getPGColTy
       , ColVals
       , PreSetCols
       , MutateResp(..)
       ) where

import           Hasura.Prelude
import qualified Hasura.SQL.DML             as S
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import qualified Data.HashMap.Strict        as Map
import qualified Data.HashMap.Strict.InsOrd as OMap
import qualified Data.Text                  as T
import qualified Database.PG.Query          as Q
import qualified Database.PostgreSQL.LibPQ  as PQ (Oid)
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)
import qualified PostgreSQL.Binary.Decoding as PD

newtype PGColOid = PGColOid { getOid :: Text }
  deriving(Show, Eq, FromJSON, ToJSON)

data PGDomBaseTyInfo
  = PGDomBaseTyInfo
  { pcdbDimension :: Integer } deriving (Show, Eq)

$(deriveJSON (aesonDrop 4 camelCase) ''PGDomBaseTyInfo )


type PGCompTyFldMap = [Map.HashMap PGTyFldName PGColOidInfo]

data PGTyInfo'
  = PGTBase
  | PGTRange
  | PGTPseudo
  | PGTArray     { pgtaElemOid        :: !PQ.Oid         }
  | PGTDomain    { pgtdBaseType       :: !PGColOidInfo   }
  | PGTEnum      { pgtePossibleValues :: ![EnumVal]      }
  | PGTComposite { pgtcFields         :: !PGCompTyFldMap }
  deriving (Show, Eq)

data PGTyInfo
  = PGTyInfo
  { ptiName    :: !QualifiedType
  , ptiOid     :: !PQ.Oid
  , ptiSqlName :: !AnnType
  , ptiDetail  :: !PGTyInfo'
  } deriving (Show, Eq)

data PGColOidInfo
  = PGColOidInfo
  { pcoiOid        :: PQ.Oid
  , pcoiDimension  :: Integer
  } deriving (Show, Eq, Generic)

instance Hashable PGColOidInfo

instance ToJSONKey PGColOidInfo

type PGTyInfoMaps =
  ( Map.HashMap PQ.Oid QualifiedType
  , Map.HashMap QualifiedType PGTyInfo
  )

mkPGTyMaps :: [PGTyInfo] -> PGTyInfoMaps
mkPGTyMaps x =
  ( Map.fromList $ flip map x $ \y -> (ptiOid y, ptiName y)
  , Map.fromList $ flip map x $ \y -> (ptiName y, y)
  )

getPGColTy :: PGTyInfoMaps  -> PGColOidInfo -> Maybe PGColType
getPGColTy maps@(oidNameMap,nameTyMap) (PGColOidInfo oid dims) = do
  PGTyInfo name _ sqlName tyDtls <- getTyOfOid oid
  fmap (PGColType name sqlName oid) $ case tyDtls of
    PGTRange       -> return PGTyRange
    PGTPseudo      -> return PGTyPseudo
    PGTBase        -> return $ PGTyBase $ txtToPgBaseColTy $ getTyText $ qName name
    PGTEnum x      -> return $ PGTyEnum x
    PGTComposite x -> fmap PGTyComposite $ mapM getSubTy $ OMap.fromList $ concatMap Map.toList x
    PGTDomain bct  -> fmap PGTyDomain $ getSubTy bct
    PGTArray bOid  -> do
      let asDimArray n y
            | n > 1     = PGTyArray $ PGColType name sqlName oid $ asDimArray (n-1) y
            | otherwise = PGTyArray y
      fmap (asDimArray dims) $ getSubTy (PGColOidInfo bOid 0)
  where
    getTyOfOid  = (flip Map.lookup oidNameMap) >=> (flip Map.lookup nameTyMap)
    getSubTy = getPGColTy maps


$(deriveJSON (aesonDrop 4 snakeCase) ''PGColOidInfo)
$(deriveJSON
  (aesonDrop 4 snakeCase)
    { constructorTagModifier = snakeCase . drop 3
    , sumEncoding = TaggedObject "type" "detail"
    }
  ''PGTyInfo')
$(deriveJSON (aesonDrop 3 snakeCase) ''PGTyInfo)

data PGColInfo
  = PGColInfo
  { pgiName       :: !PGCol
  , pgiType       :: !PGColType
  , pgiIsNullable :: !Bool
  } deriving (Show, Eq)

$(deriveJSON (aesonDrop 3 snakeCase) ''PGColInfo)

pattern JSONColInfo :: PGCol -> Bool -> QualifiedType -> AnnType -> PQ.Oid -> PGColInfo
pattern JSONColInfo a b x y z = PGColInfo a (PGColType x y z (PGTyBase PGJSON)) b

newtype RelName
  = RelName {getRelTxt :: T.Text}
  deriving (Show, Eq, Hashable, FromJSON, ToJSON, Q.ToPrepArg, Q.FromCol, Lift)

instance IsIden RelName where
  toIden (RelName r) = Iden r

instance DQuote RelName where
  dquoteTxt (RelName r) = r

relTypeToTxt :: RelType -> T.Text
relTypeToTxt ObjRel = "object"
relTypeToTxt ArrRel = "array"

data RelType
  = ObjRel
  | ArrRel
  deriving (Show, Eq)

instance ToJSON RelType where
  toJSON = String . relTypeToTxt

instance FromJSON RelType where
  parseJSON (String "object") = return ObjRel
  parseJSON (String "array") = return ArrRel
  parseJSON _ = fail "expecting either 'object' or 'array' for rel_type"

instance Q.FromCol RelType where
  fromCol bs = flip Q.fromColHelper bs $ PD.enum $ \case
    "object" -> Just ObjRel
    "array"  -> Just ArrRel
    _   -> Nothing

data RelInfo
  = RelInfo
  { riName     :: !RelName
  , riType     :: !RelType
  , riMapping  :: ![(PGCol, PGCol)]
  , riRTable   :: !QualifiedTable
  , riIsManual :: !Bool
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 2 snakeCase) ''RelInfo)

newtype FieldName
  = FieldName { getFieldNameTxt :: T.Text }
  deriving (Show, Eq, Ord, Hashable, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Lift)

instance IsIden FieldName where
  toIden (FieldName f) = Iden f

instance DQuote FieldName where
  dquoteTxt (FieldName c) = c

fromPGCol :: PGCol -> FieldName
fromPGCol (PGCol c) = FieldName c

fromRel :: RelName -> FieldName
fromRel (RelName r) = FieldName r

newtype TQueryName
  = TQueryName { getTQueryName :: T.Text }
  deriving ( Show, Eq, Hashable, FromJSONKey, ToJSONKey
           , FromJSON, ToJSON, Q.ToPrepArg, Q.FromCol, Lift)

instance IsIden TQueryName where
  toIden (TQueryName r) = Iden r

instance DQuote TQueryName where
  dquoteTxt (TQueryName r) = r

newtype TemplateParam
  = TemplateParam { getTemplateParam :: T.Text }
  deriving (Show, Eq, Hashable, FromJSON, FromJSONKey, ToJSONKey, ToJSON, Lift)

instance DQuote TemplateParam where
  dquoteTxt (TemplateParam r) = r

class ToAesonPairs a where
  toAesonPairs :: (KeyValue v) => a -> [v]

data WithTable a
  = WithTable
  { wtName :: !QualifiedTable
  , wtInfo :: !a
  } deriving (Show, Eq, Lift)

instance (FromJSON a) => FromJSON (WithTable a) where
  parseJSON v@(Object o) =
    WithTable <$> o .: "table" <*> parseJSON v
  parseJSON _ =
    fail "expecting an Object with key 'table'"

instance (ToAesonPairs a) => ToJSON (WithTable a) where
  toJSON (WithTable tn rel) =
    object $ ("table" .= tn):toAesonPairs rel

type ColVals = Map.HashMap PGCol Value
type PreSetCols = Map.HashMap PGCol S.SQLExp

data MutateResp
  = MutateResp
  { _mrAffectedRows     :: !Int
  , _mrReturningColumns :: ![ColVals]
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase) ''MutateResp)
