module Hasura.GraphQL.Parser.Directives
  ( customDirectives
  , parseDirectives
  , withDirective
  ) where

import           Hasura.Prelude

import qualified Data.Dependent.Map                    as DM
import qualified Language.GraphQL.Draft.Syntax         as G

import           Type.Reflection                       (Typeable)

import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Schema


customDirectives :: forall m. MonadParse m => [Directive m]


parseDirectives
  :: forall m. MonadParse m
  => [Directive m]
  -> G.DirectiveLocation
  -> [G.Directive Variable]
  -> m (DM.DMap DirectiveKey Identity)

withDirective
  :: DM.DMap DirectiveKey Identity
  -> DirectiveKey a
  -> (Maybe a -> m b)
  -> m b


data Directive m where
  Directive :: forall m a. (MonadParse m, Typeable a) =>
    { dDefinition :: DirectiveInfo
    , dParser     :: G.Directive Variable -> m a
    } -> Directive m

data DirectiveKey a where
  DirectiveKey :: Typeable a => G.Name -> DirectiveKey a
