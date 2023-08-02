module Hasura.Services (module Services, ProvidesHasuraServices) where

import Hasura.Services.Network as Services

{- Note [Services]

Different editions of the GraphQL Engine use the same common core, but provide
different features. To avoid having logic deep within the common core that
decides whether a feature is active or not, we favour an "injection" approach:
the core of the engine delegates the implementation details of features /
external dependencies to class constraints, and it's the role of the top-level
caller to implement those constraints.

Those services are implemented on the base monad on which we run the engine. See
'PGMetadataStorageT' in Hasura/App.

-}

-- | A constraint alias that groups all services together.
type ProvidesHasuraServices m =
  ( ProvidesNetwork m
  )
