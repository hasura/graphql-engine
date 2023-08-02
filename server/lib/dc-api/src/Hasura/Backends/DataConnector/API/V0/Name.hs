{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use onNothing" #-}

module Hasura.Backends.DataConnector.API.V0.Name (nameCodec) where

import Autodocodec
import Data.Text qualified as Text
import Language.GraphQL.Draft.Syntax qualified as GQL
import Prelude

nameCodec :: JSONCodec GQL.Name
nameCodec =
  bimapCodec
    parseName
    GQL.unName
    (StringCodec (Just "GraphQLName"))
    <?> "A valid GraphQL name"
  where
    parseName text = maybe (Left $ Text.unpack text <> " is not a valid GraphQL name") pure $ GQL.mkName text
