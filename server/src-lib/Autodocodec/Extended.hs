module Autodocodec.Extended
  ( optionalFieldOrIncludedNull,
    optionalFieldOrIncludedNull',
    optionalFieldOrIncludedNullWith,
    optionalFieldOrIncludedNullWith',
  )
where

import Autodocodec
import Hasura.Prelude

-- | An optional field that might be @null@ where a @Nothing@ value should be
-- represented as @null@ on serialization instead of omitting the field.
--
-- This differs from Autodocodec's stock 'optionalFieldOrNull' in that that
-- function omits the field during serialization if the Haskell value is
-- @Nothing@. This version includes the field with a serialized value of @null@.
optionalFieldOrIncludedNull ::
  HasCodec output =>
  -- | Key
  Text ->
  -- | Documentation
  Text ->
  ObjectCodec (Maybe output) (Maybe output)
optionalFieldOrIncludedNull key doc = optionalFieldOrIncludedNullWith key codec doc

-- | An optional field that might be @null@ where a @Nothing@ value should be
-- represented as @null@ on serialization instead of omitting the field.
--
-- This differs from Autodocodec's stock 'optionalFieldOrNull'' in that that
-- function omits the field during serialization if the Haskell value is
-- @Nothing@. This version includes the field with a serialized value of @null@.
optionalFieldOrIncludedNull' ::
  HasCodec output =>
  -- | Key
  Text ->
  ObjectCodec (Maybe output) (Maybe output)
optionalFieldOrIncludedNull' key = optionalFieldOrIncludedNullWith' key codec

-- | An optional field that might be @null@ where a @Nothing@ value should be
-- represented as @null@ on serialization instead of omitting the field.
--
-- This differs from Autodocodec's stock 'optionalFieldOrNullWith' in that that
-- function omits the field during serialization if the Haskell value is
-- @Nothing@. This version includes the field with a serialized value of @null@.
optionalFieldOrIncludedNullWith ::
  -- | Key
  Text ->
  -- | Codec for the value
  JSONCodec output ->
  -- | Documentation
  Text ->
  ObjectCodec (Maybe output) (Maybe output)
optionalFieldOrIncludedNullWith key c doc =
  orIncludedNullHelper $
    OptionalKeyCodec key (maybeCodec c) (Just doc)

-- | An optional field that might be @null@ where a @Nothing@ value should be
-- represented as @null@ on serialization instead of omitting the field.
--
-- This differs from Autodocodec's stock 'optionalFieldOrNullWith'' in that that
-- function omits the field during serialization if the Haskell value is
-- @Nothing@. This version includes the field with a serialized value of @null@.
optionalFieldOrIncludedNullWith' ::
  -- | Key
  Text ->
  -- | Codec for the value
  JSONCodec output ->
  ObjectCodec (Maybe output) (Maybe output)
optionalFieldOrIncludedNullWith' key c =
  orIncludedNullHelper $
    OptionalKeyCodec key (maybeCodec c) Nothing

orIncludedNullHelper :: ObjectCodec (Maybe (Maybe input)) (Maybe (Maybe output)) -> ObjectCodec (Maybe input) (Maybe output)
orIncludedNullHelper = dimapCodec dec enc
  where
    dec :: Maybe (Maybe input) -> Maybe input
    dec = \case
      Nothing -> Nothing
      Just Nothing -> Nothing
      Just (Just a) -> Just a
    enc :: Maybe output -> Maybe (Maybe output)
    enc = \case
      Nothing -> Just Nothing -- This is the case that differs from the stock `orNullHelper`
      Just a -> Just (Just a)
