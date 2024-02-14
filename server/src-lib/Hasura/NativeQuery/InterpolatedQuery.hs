{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Parser and prettyprinter for native query code.
module Hasura.NativeQuery.InterpolatedQuery
  ( ArgumentName (..),
    InterpolatedItem (..),
    InterpolatedQuery (..),
    parseInterpolatedQuery,
    getUniqueVariables,
    trimQueryEnd,
    ppInterpolatedQuery,
    module Hasura.LogicalModel.NullableScalarType,
  )
where

import Autodocodec
import Autodocodec qualified as AC
import Control.Lens (over, _last)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Hasura.LogicalModel.NullableScalarType (NullableScalarType (..), nullableScalarTypeMapCodec)
import Hasura.LogicalModelResolver.Types (ArgumentName (..))
import Hasura.Prelude hiding (first)
import Language.Haskell.TH.Syntax (Lift)

newtype RawQuery = RawQuery {getRawQuery :: Text}
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSON)

instance HasCodec RawQuery where
  codec = AC.dimapCodec RawQuery getRawQuery codec

---------------------------------------

-- | A component of an interpolated query
data InterpolatedItem variable
  = -- | normal text
    IIText Text
  | -- | a captured variable
    IIVariable variable
  deriving stock (Eq, Ord, Show, Functor, Foldable, Data, Generic, Lift, Traversable)

-- | Converting an interpolated query back to text.
--   Should roundtrip with the 'parseInterpolatedQuery'.
ppInterpolatedItem :: InterpolatedItem ArgumentName -> Text
ppInterpolatedItem (IIText t) = t
ppInterpolatedItem (IIVariable v) = "{{" <> getArgumentName v <> "}}"

deriving instance (Hashable variable) => Hashable (InterpolatedItem variable)

deriving instance (NFData variable) => NFData (InterpolatedItem variable)

---------------------------------------

-- | A list of stored procedure components representing a single stored procedure,
--   separating the variables from the text.
newtype InterpolatedQuery variable = InterpolatedQuery
  { getInterpolatedQuery :: [InterpolatedItem variable]
  }
  deriving newtype (Eq, Ord, Show, Generic)
  deriving stock (Data, Functor, Foldable, Lift, Traversable)

deriving newtype instance (Hashable variable) => Hashable (InterpolatedQuery variable)

deriving newtype instance (NFData variable) => NFData (InterpolatedQuery variable)

ppInterpolatedQuery :: InterpolatedQuery ArgumentName -> Text
ppInterpolatedQuery (InterpolatedQuery parts) = foldMap ppInterpolatedItem parts

-- | We store the interpolated query as the user text and parse it back
--   when converting back to Haskell code.
instance (v ~ ArgumentName) => HasCodec (InterpolatedQuery v) where
  codec =
    CommentCodec
      ("An interpolated query expressed in native code (SQL)")
      $ bimapCodec
        (first T.unpack . parseInterpolatedQuery)
        ppInterpolatedQuery
        textCodec

-- This instance is for Metadata, do another instance for agents, or create a newtype.
deriving via
  (Autodocodec (InterpolatedQuery ArgumentName))
  instance
    (v ~ ArgumentName) =>
    ToJSON (InterpolatedQuery v)

---------------------------------------

-- | extract all of the `{{ variable }}` inside our query string
parseInterpolatedQuery ::
  Text ->
  Either Text (InterpolatedQuery ArgumentName)
parseInterpolatedQuery =
  fmap
    ( InterpolatedQuery
        . mergeAdjacent
        . trashEmpties
    )
    . consumeString
    . T.unpack
  where
    trashEmpties = filter (/= IIText "")

    mergeAdjacent = \case
      (IIText a : IIText b : rest) ->
        mergeAdjacent (IIText (a <> b) : rest)
      (a : rest) -> a : mergeAdjacent rest
      [] -> []

    consumeString :: String -> Either Text [InterpolatedItem ArgumentName]
    consumeString str =
      let (beforeCurly, fromCurly) = break (== '{') str
       in case fromCurly of
            ('{' : '{' : rest) ->
              (IIText (T.pack beforeCurly) :) <$> consumeVar rest
            ('{' : other) ->
              (IIText (T.pack (beforeCurly <> "{")) :) <$> consumeString other
            _other -> pure [IIText (T.pack beforeCurly)]

    consumeVar :: String -> Either Text [InterpolatedItem ArgumentName]
    consumeVar str =
      let (beforeCloseCurly, fromClosedCurly) = break (== '}') str
       in case fromClosedCurly of
            ('}' : '}' : rest) ->
              (IIVariable (ArgumentName $ T.pack beforeCloseCurly) :) <$> consumeString rest
            _ -> Left "Found '{{' without a matching closing '}}'"

-- | Get a set of all arguments used in an interpolated query.
getUniqueVariables :: (Ord var) => InterpolatedQuery var -> Set var
getUniqueVariables (InterpolatedQuery items) =
  flip foldMap items \case
    IIText _ -> mempty
    IIVariable variable -> Set.singleton variable

-- | Remove spaces and semicolon from the end of a query and add a newline, for sql backends.
trimQueryEnd :: InterpolatedQuery var -> InterpolatedQuery var
trimQueryEnd (InterpolatedQuery parts) =
  InterpolatedQuery
    $ over _last dropIt parts
    -- if the user has a comment on the last line, this will make sure it doesn't interrupt the rest of the query
    <> [IIText "\n"]
  where
    dropIt = \case
      IIText txt ->
        IIText
          . T.dropWhileEnd (== ';')
          . T.dropWhileEnd (\c -> c == ' ' || c == '\t' || c == '\n')
          $ txt
      IIVariable v -> IIVariable v
