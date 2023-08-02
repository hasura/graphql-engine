-- | This module contains functions that help express expectations about json
-- values.
module Test.Aeson.Expectation
  ( shouldBeSubsetOf,
    jsonSubsetOf,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Encode.Pretty qualified as AP
import Data.Aeson.KeyMap qualified as J
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.These
import Data.Vector qualified as V
import Hasura.Backends.Postgres.Instances.Schema ()
import Hasura.Prelude
import Test.Hspec

-- | Assert that one json value should be a subset of another, in the sense of 'jsonSubsetOf'.
shouldBeSubsetOf :: J.Value -> J.Value -> IO ()
shouldBeSubsetOf subset superset | subset `jsonSubsetOf` superset = return ()
shouldBeSubsetOf subset superset =
  expectationFailure
    $ T.unpack
    $ decodeUtf8
    $ LBS.toStrict
    $ AP.encodePretty subset
    <> " is not a subset of "
    <> AP.encodePretty superset

-- | Compute whether one json value 'sub' is a subset of another value 'sup', in the sense that:
--
-- * For arrays, there is a contiguous segment in 'sup' in which all elements are subset-related with 'sub' in order
-- * For objects, the keys of 'sub' are a subset of those of 'sup', and all their associated values are also subset-related
-- * Leaf values are identical
jsonSubsetOf :: J.Value -> J.Value -> Bool
jsonSubsetOf (J.Array sub) (J.Array sup) = sub `subarrayOf` sup
jsonSubsetOf (J.Object sub) (J.Object sup) = sub `subobjectOf` sup
jsonSubsetOf (J.String sub) (J.String sup) = sub == sup
jsonSubsetOf (J.Number sub) (J.Number sup) = sub == sup
jsonSubsetOf (J.Bool sub) (J.Bool sup) = sub == sup
jsonSubsetOf J.Null J.Null = True
jsonSubsetOf _sub _sup = False

subobjectOf :: J.KeyMap J.Value -> J.KeyMap J.Value -> Bool
subobjectOf sub sup =
  J.foldr (&&) True
    $ J.alignWith
      ( \case
          This _ -> False -- key is only in the sub
          That _ -> True -- key is only in sup
          These l r -> l `jsonSubsetOf` r
      )
      sub
      sup

subarrayOf :: V.Vector J.Value -> V.Vector J.Value -> Bool
subarrayOf sub sup | V.length sub > V.length sup = False
subarrayOf sub sup | V.and $ V.zipWith jsonSubsetOf sub sup = True
subarrayOf sub sup = subarrayOf sub (V.tail sup)
