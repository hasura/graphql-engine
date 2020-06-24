module Hasura.GraphQL.Parser.Internal.Parser where

import           Hasura.Prelude

import           Language.GraphQL.Draft.Syntax

import {-# SOURCE #-} Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Schema

type role Parser nominal representational nominal
data Parser (k :: Kind) (m :: * -> *) (a :: *)

runParser :: Parser k m a -> ParserInput k -> m a

type family ParserInput k where
  ParserInput 'Both = Value Variable
  ParserInput 'Input = Value Variable
  ParserInput 'Output = SelectionSet NoFragments Variable

boolean :: MonadParse m => Parser 'Both m Bool
