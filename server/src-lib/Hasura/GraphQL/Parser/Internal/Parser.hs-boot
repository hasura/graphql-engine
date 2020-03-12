module Hasura.GraphQL.Parser.Internal.Parser where

import           Hasura.GraphQL.Parser.Schema

type role Parser nominal representational nominal
data Parser (k :: Kind) (m :: * -> *) (a :: *)
