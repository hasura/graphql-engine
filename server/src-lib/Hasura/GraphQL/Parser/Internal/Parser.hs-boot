module Hasura.GraphQL.Parser.Internal.Parser where

import           Hasura.Prelude

import qualified Data.Kind                     as K

import           Language.GraphQL.Draft.Syntax


import {-# SOURCE #-} Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Schema

type role Parser nominal representational nominal
data Parser (k :: Kind) (m :: K.Type -> K.Type) (a :: K.Type)

runParser :: Parser k m a -> ParserInput k -> m a

type family ParserInput k where
  ParserInput 'Both = InputValue Variable
  ParserInput 'Input = InputValue Variable
  ParserInput 'Output = SelectionSet NoFragments Variable

boolean :: MonadParse m => Parser 'Both m Bool
