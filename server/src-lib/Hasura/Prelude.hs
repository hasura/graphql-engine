module Hasura.Prelude
  ( module M
  , onNothing
  , onJust
  , onLeft
  , bsToTxt
  , txtToBs
  ) where

import           Control.Applicative        as M ((<|>))
import           Control.Monad              as M (void, when)
import           Control.Monad.Base         as M
import           Control.Monad.Except       as M
import           Control.Monad.Fail         as M (MonadFail)
import           Control.Monad.Identity     as M
import           Control.Monad.Reader       as M
import           Control.Monad.State.Strict as M
import           Data.Bool                  as M (bool)
import           Data.Either                as M (lefts, partitionEithers,
                                                  rights)
import           Data.Foldable              as M (foldrM, toList)
import           Data.Functor               as M (($>), (<&>))
import           Data.Hashable              as M (Hashable)
import           Data.List                  as M (find, foldl', group,
                                                  intercalate, intersect,
                                                  lookup, sort, sortBy, sortOn,
                                                  union, unionBy, (\\))
import           Data.Maybe                 as M (catMaybes, fromMaybe, isJust,
                                                  isNothing, listToMaybe,
                                                  mapMaybe, maybeToList)
import           Data.Ord                   as M (comparing)
import           Data.Semigroup             as M (Semigroup (..))
import           Data.String                as M (IsString)
import           Data.Text                  as M (Text)
import           Data.Word                  as M (Word64)
import           GHC.Generics               as M (Generic)
import           Prelude                    as M hiding (fail, init, lookup)
import           Text.Read                  as M (readEither, readMaybe)

import qualified Data.ByteString            as B
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Encoding.Error   as TE

onNothing :: (Monad m) => Maybe a -> m a -> m a
onNothing m act = maybe act return m

onJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
onJust m action = maybe (return ()) action m

onLeft :: (Monad m) => Either e a -> (e -> m a) -> m a
onLeft e f = either f return e

bsToTxt :: B.ByteString -> Text
bsToTxt = TE.decodeUtf8With TE.lenientDecode

txtToBs :: Text -> B.ByteString
txtToBs = TE.encodeUtf8
