{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.BigQuery.Name
  ( _Bytes,
    _Date,
    _Datetime,
    _Geography,
    _Time,
    _Timestamp,
  )
where

import Language.GraphQL.Draft.Syntax qualified as G
import Language.GraphQL.Draft.Syntax.QQ qualified as G

_Bytes :: G.Name
_Bytes = [G.name|Bytes|]

_Date :: G.Name
_Date = [G.name|Date|]

_Datetime :: G.Name
_Datetime = [G.name|Datetime|]

_Geography :: G.Name
_Geography = [G.name|Geography|]

_Time :: G.Name
_Time = [G.name|Time|]

_Timestamp :: G.Name
_Timestamp = [G.name|Timestamp|]
