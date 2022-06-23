{-# LANGUAGE QuasiQuotes #-}

module Hasura.Server.Name
  ( _Bool,
    _Double,
    _float8,
    _Number,
    _numeric,
  )
where

import Language.GraphQL.Draft.Syntax qualified as G
import Language.GraphQL.Draft.Syntax.QQ qualified as G

_Bool :: G.Name
_Bool = [G.name|Bool|]

_Double :: G.Name
_Double = [G.name|Double|]

_float8 :: G.Name
_float8 = [G.name|float8|]

_Number :: G.Name
_Number = [G.name|Number|]

_numeric :: G.Name
_numeric = [G.name|numeric|]
