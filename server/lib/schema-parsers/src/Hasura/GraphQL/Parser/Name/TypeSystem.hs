{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Hasura.GraphQL.Parser.Name.TypeSystem where

import Language.GraphQL.Draft.Syntax qualified as G
import Language.GraphQL.Draft.Syntax.QQ qualified as G

-- * 3. Type System

-- ** 3.3. Schema

-- *** 3.3.1. Root Operation Types

_Mutation :: G.Name
_Mutation = [G.name|Mutation|]

_Query :: G.Name
_Query = [G.name|Query|]

_Subscription :: G.Name
_Subscription = [G.name|Subscription|]

-- ** 3.5. Scalars

_Int :: G.Name
_Int = [G.name|Int|]

_Float :: G.Name
_Float = [G.name|Float|]

_String :: G.Name
_String = [G.name|String|]

_Boolean :: G.Name
_Boolean = [G.name|Boolean|]

_ID :: G.Name
_ID = [G.name|ID|]
