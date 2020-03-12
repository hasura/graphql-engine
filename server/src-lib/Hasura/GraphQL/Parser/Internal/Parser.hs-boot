module Hasura.GraphQL.Parser.Internal.Parser where

import           Hasura.GraphQL.Parser.Internal.Schema

type role Parser nominal representational nominal
data Parser (k :: Kind) (m :: * -> *) (a :: *)
