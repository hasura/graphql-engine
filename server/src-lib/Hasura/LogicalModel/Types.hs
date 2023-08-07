{-# LANGUAGE DeriveAnyClass #-}

-- | A name for a logical model as it is recognized by the graphql schema.
module Hasura.LogicalModel.Types
  ( LogicalModelName (..),
    LogicalModelField (..),
    LogicalModelFields,
    LogicalModelType (..),
    LogicalModelTypeScalar (..),
    LogicalModelTypeArray (..),
    LogicalModelTypeReference (..),
    logicalModelFieldMapCodec,
    LogicalModelLocation (..),
  )
where

import Autodocodec
  ( HasCodec (codec),
    dimapCodec,
  )
import Autodocodec qualified as AC
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, Value)
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Text.Extended (ToTxt (..))
import Hasura.Metadata.DTO.Placeholder (placeholderCodecViaJSON)
import Hasura.NativeQuery.Types (NativeQueryName)
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.BackendTag (backendPrefix)
import Language.GraphQL.Draft.Syntax qualified as G
import Language.Haskell.TH.Syntax (Lift)

-- The name of a logical model. This appears as a root field name in the graphql schema.
newtype LogicalModelName = LogicalModelName {getLogicalModelName :: G.Name}
  deriving newtype (Eq, Ord, Show, Hashable, NFData, ToJSON, FromJSON, ToTxt)
  deriving stock (Data, Generic, Lift)

instance HasCodec LogicalModelName where
  codec = dimapCodec LogicalModelName getLogicalModelName codec

instance FromJSONKey LogicalModelName

instance ToJSONKey LogicalModelName

----

data LogicalModelTypeScalar b = LogicalModelTypeScalarC
  { lmtsScalar :: ScalarType b,
    lmtsNullable :: Bool
  }
  deriving (Generic)

deriving stock instance (Backend b) => Eq (LogicalModelTypeScalar b)

deriving stock instance (Backend b) => Show (LogicalModelTypeScalar b)

instance (Backend b) => Hashable (LogicalModelTypeScalar b)

instance (Backend b) => NFData (LogicalModelTypeScalar b)

instance (Backend b) => HasCodec (LogicalModelTypeScalar b) where
  codec =
    AC.CommentCodec
      ("A scalar type used in a Logical Model.")
      $ AC.object (backendPrefix @b <> "LogicalModelTypeScalar")
      $ LogicalModelTypeScalarC
      <$> AC.requiredField "scalar" scalarDoc
      AC..= lmtsScalar
        <*> AC.optionalFieldWithDefault "nullable" False nullableDoc
      AC..= lmtsNullable
    where
      scalarDoc = "Name of the scalar type"
      nullableDoc = "Whether this field is allowed to contain null values or not"

deriving via
  (AC.Autodocodec (LogicalModelTypeScalar b))
  instance
    (Backend b) => (FromJSON (LogicalModelTypeScalar b))

deriving via
  (AC.Autodocodec (LogicalModelTypeScalar b))
  instance
    (Backend b) => (ToJSON (LogicalModelTypeScalar b))

----

data LogicalModelTypeArray b = LogicalModelTypeArrayC
  { lmtaArray :: LogicalModelType b,
    lmtaNullable :: Bool
  }
  deriving (Generic)

deriving stock instance (Backend b) => Eq (LogicalModelTypeArray b)

deriving stock instance (Backend b) => Show (LogicalModelTypeArray b)

instance (Backend b) => Hashable (LogicalModelTypeArray b)

instance (Backend b) => NFData (LogicalModelTypeArray b)

instance (Backend b) => HasCodec (LogicalModelTypeArray b) where
  codec =
    AC.CommentCodec
      ("An array type used in a Logical Model.")
      $ AC.object (backendPrefix @b <> "LogicalModelTypeArray")
      $ LogicalModelTypeArrayC
      <$> AC.requiredField "array" arrayDoc
      AC..= lmtaArray
        <*> AC.optionalFieldWithDefault "nullable" False nullableDoc
      AC..= lmtaNullable
    where
      arrayDoc = "Type of items inside array"
      nullableDoc = "Whether this field can be null or not"

deriving via
  (AC.Autodocodec (LogicalModelTypeArray b))
  instance
    (Backend b) => (FromJSON (LogicalModelTypeArray b))

deriving via
  (AC.Autodocodec (LogicalModelTypeArray b))
  instance
    (Backend b) => (ToJSON (LogicalModelTypeArray b))

----

data LogicalModelTypeReference = LogicalModelTypeReferenceC
  { lmtrReference :: LogicalModelName,
    lmtrNullable :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

instance HasCodec LogicalModelTypeReference where
  codec =
    AC.CommentCodec
      ("A reference to another Logical Model.")
      $ AC.object "LogicalModelTypeReference"
      $ LogicalModelTypeReferenceC
      <$> AC.requiredField "logical_model" referenceDoc
      AC..= lmtrReference
        <*> AC.optionalFieldWithDefault "nullable" False nullableDoc
      AC..= lmtrNullable
    where
      referenceDoc = "Name of another Logical Model to nest"
      nullableDoc = "Whether this field can be null or not"

deriving via
  (AC.Autodocodec LogicalModelTypeReference)
  instance
    (FromJSON LogicalModelTypeReference)

deriving via
  (AC.Autodocodec LogicalModelTypeReference)
  instance
    (ToJSON LogicalModelTypeReference)

----

data LogicalModelType b
  = LogicalModelTypeScalar (LogicalModelTypeScalar b)
  | LogicalModelTypeArray (LogicalModelTypeArray b)
  | LogicalModelTypeReference LogicalModelTypeReference
  deriving (Generic)

deriving stock instance (Backend b) => Eq (LogicalModelType b)

deriving stock instance (Backend b) => Show (LogicalModelType b)

instance (Backend b) => Hashable (LogicalModelType b)

instance (Backend b) => NFData (LogicalModelType b)

-- | forgive me, I really did try and do this the native Autodocodec way
-- and everything I did kept freezing the whole of HGE
instance (Backend b) => HasCodec (LogicalModelType b) where
  codec =
    AC.CommentCodec
      ("A type used in a Logical Model field")
      $ placeholderCodecViaJSON

instance (Backend b) => FromJSON (LogicalModelType b) where
  parseJSON j =
    (LogicalModelTypeScalar <$> parseJSON j)
      <|> (LogicalModelTypeArray <$> parseJSON j)
      <|> (LogicalModelTypeReference <$> parseJSON j)

instance (Backend b) => ToJSON (LogicalModelType b) where
  toJSON (LogicalModelTypeScalar t) = toJSON t
  toJSON (LogicalModelTypeArray t) = toJSON t
  toJSON (LogicalModelTypeReference t) = toJSON t

----

-- | a single field in a Logical Model
data LogicalModelField b = LogicalModelField
  { lmfName :: Column b,
    lmfType :: LogicalModelType b,
    lmfDescription :: Maybe Text
  }
  deriving (Generic)

data LogicalModelFieldSimple b = LogicalModelFieldSimple
  { lmfsName :: Column b,
    lmfsScalar :: ScalarType b,
    lmfsNullable :: Bool,
    lmfsDescription :: Maybe Text
  }

-- | this codec is complicated because we want to support both the old scalar
-- encoded fields and our new separate type
instance (Backend b) => HasCodec (LogicalModelField b) where
  codec = AC.parseAlternative newCodec simpleCodecMappedToNew -- we always encode as `newCodec` but we try parsing with both
    where
      -- if we parse the old kind, convert it to the new exciting kind
      fromSimple :: LogicalModelFieldSimple b -> LogicalModelField b
      fromSimple (LogicalModelFieldSimple {lmfsName, lmfsScalar, lmfsNullable, lmfsDescription}) =
        LogicalModelField
          { lmfName = lmfsName,
            lmfDescription = lmfsDescription,
            lmfType =
              LogicalModelTypeScalar
                ( LogicalModelTypeScalarC {lmtsScalar = lmfsScalar, lmtsNullable = lmfsNullable}
                )
          }

      -- try and convert the new kind to the old (this is partial, but
      -- shouldn't actually be used)
      toSimple
        ( LogicalModelField
            { lmfName,
              lmfDescription,
              lmfType = LogicalModelTypeScalar (LogicalModelTypeScalarC {lmtsScalar, lmtsNullable})
            }
          ) =
          LogicalModelFieldSimple
            { lmfsName = lmfName,
              lmfsScalar = lmtsScalar,
              lmfsNullable = lmtsNullable,
              lmfsDescription = lmfDescription
            }
      toSimple _ = error "Could not convert LogicalModelField to LogicalModelFieldSimple"

      simpleCodecMappedToNew :: AC.JSONCodec (LogicalModelField b)
      simpleCodecMappedToNew = AC.dimapCodec fromSimple toSimple simpleCodec

      simpleCodec :: AC.JSONCodec (LogicalModelFieldSimple b)
      simpleCodec =
        -- this is the simpler old codec that did scalar types only
        AC.CommentCodec
          ("A field of a logical model")
          $ AC.object (backendPrefix @b <> "LogicalModelField")
          $ LogicalModelFieldSimple
          <$> AC.requiredField "name" nameDoc
          AC..= lmfsName
            <*> AC.requiredField "type" typeDoc
          AC..= lmfsScalar
            <*> AC.optionalFieldWithDefault "nullable" False nullableDoc
          AC..= lmfsNullable
            <*> AC.optionalField "description" descriptionDoc
          AC..= lmfsDescription
        where
          nameDoc = "Name of the field"
          nullableDoc = "Is field nullable or not?"
          typeDoc = "Type of the field"
          descriptionDoc = "Optional description of this field"

      newCodec =
        -- the new codec which defers to LogicalModelType for all the
        -- complexities
        AC.CommentCodec
          ("A field of a logical model")
          $ AC.object (backendPrefix @b <> "LogicalModelField")
          $ LogicalModelField
          <$> AC.requiredField "name" nameDoc
          AC..= lmfName
            <*> AC.requiredField "type" typeDoc
          AC..= lmfType
            <*> AC.optionalField "description" descriptionDoc
          AC..= lmfDescription
        where
          nameDoc = "Name of the field"
          typeDoc = "Type of the field"
          descriptionDoc = "Optional description of this field"

deriving via
  (AC.Autodocodec (LogicalModelField b))
  instance
    (Backend b) => (ToJSON (LogicalModelField b))

deriving via
  (AC.Autodocodec (LogicalModelField b))
  instance
    (Backend b) => FromJSON (LogicalModelField b)

deriving stock instance (Backend b) => Eq (LogicalModelField b)

deriving stock instance (Backend b) => Show (LogicalModelField b)

instance (Backend b) => Hashable (LogicalModelField b)

instance (Backend b) => NFData (LogicalModelField b)

type LogicalModelFields b = InsOrdHashMap.InsOrdHashMap (Column b) (LogicalModelField b)

-- we parse in as an array of NullableScalarTypeFromArray and then turn into
-- InsOrdHashMap because JSON objects cannot be depended on for ordering
logicalModelFieldMapCodec ::
  forall b.
  (Backend b) =>
  AC.Codec
    Value
    (LogicalModelFields b)
    (LogicalModelFields b)
logicalModelFieldMapCodec =
  AC.dimapCodec
    ( InsOrdHashMap.fromList
        . fmap
          ( \lmf -> (lmfName lmf, lmf)
          )
    )
    ( fmap snd . InsOrdHashMap.toList
    )
    (AC.codec @[LogicalModelField b])

-- when we are talking about permissions, they might be attached directly to a
-- Native Query or similar
data LogicalModelLocation
  = LMLLogicalModel LogicalModelName
  | LMLNativeQuery NativeQueryName
  deriving (Eq, Ord, Show, Generic, Hashable)

instance ToTxt LogicalModelLocation where
  toTxt (LMLLogicalModel lmn) = toTxt lmn
  toTxt (LMLNativeQuery nqn) = toTxt nqn
