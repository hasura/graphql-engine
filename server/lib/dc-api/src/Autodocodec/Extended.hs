module Autodocodec.Extended
  ( DisjunctCodec (..),
    disjointMatchChoicesNECodec,
    module Autodocodec,
  )
where

import Autodocodec
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe (fromJust)
import Prelude

data DisjunctCodec context newInput output where
  DisjunctCodec :: (newInput -> Maybe input) -> Codec context input output -> DisjunctCodec context newInput output

-- | A choice codec for a disjoint non-empty list of options
-- Note that this list of options must be complete.
-- There is a variant of newInput for which a DisjunctCodec is not provided
-- then encoding may fail with a call to `error` (via `fromJust`)
disjointMatchChoicesNECodec ::
  -- | Codecs, each which their own rendering matcher
  NonEmpty (DisjunctCodec context newInput output) ->
  Codec context newInput output
disjointMatchChoicesNECodec l = go l
  where
    go (DisjunctCodec m c :| rest) = case nonEmpty rest of
      Nothing -> lmapCodec (fromJust . m) c
      Just l' ->
        disjointMatchChoiceCodec c (go l') $ \i -> case m i of
          Just j -> Left j
          Nothing -> Right i
