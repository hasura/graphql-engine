{-# LANGUAGE QuasiQuotes #-}

module Hasura.GraphQL.Parser.DirectiveName
  ( _cached,
    _if,
    _include,
    _refresh,
    _skip,
    _ttl,
    __multiple_top_level_fields,
  )
where

import Language.GraphQL.Draft.Syntax as G
import Language.GraphQL.Draft.Syntax.QQ as G

_cached :: G.Name
_cached = [G.name|cached|]

_if :: G.Name
_if = [G.name|if|]

_include :: G.Name
_include = [G.name|include|]

_refresh :: G.Name
_refresh = [G.name|refresh|]

_skip :: G.Name
_skip = [G.name|skip|]

_ttl :: G.Name
_ttl = [G.name|ttl|]

__multiple_top_level_fields :: G.Name
__multiple_top_level_fields = [G.name|_multiple_top_level_fields|]
