{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.GraphQL.Instances where

import           Hasura.Prelude

import qualified Data.Aeson                              as J
import qualified Data.Text.Lazy                          as TL
import qualified Language.GraphQL.Draft.Parser           as G
import qualified Language.GraphQL.Draft.Printer.LazyText as GPL
import qualified Language.GraphQL.Draft.Syntax           as G

instance J.FromJSON G.ExecutableDocument where
  parseJSON = J.withText "ExecutableDocument" $ \t ->
    onLeft (G.parseExecutableDoc t) $
    const $ fail "parsing the graphql query failed"

instance J.ToJSON G.ExecutableDocument where
  toJSON = J.String . TL.toStrict . GPL.renderExecutableDoc
