{-# LANGUAGE TemplateHaskell #-}

module Hasura.Function.Lenses
  ( fiComment,
    fiDescription,
    fiExposedAs,
    fiGQLAggregateName,
    fiGQLArgsName,
    fiGQLName,
    fiInputArgs,
    fiJsonAggSelect,
    fiPermissions,
    fiReturnType,
    fiSQLName,
    fiSystemDefined,
    fiVolatility,
    fpmRole,
    _IASessionVariables,
    _IAUserProvided,
  )
where

import Control.Lens (makeLenses, makePrisms)
import Hasura.Function.Cache (FunctionInfo (..), FunctionPermissionInfo (..), InputArgument (..))

$(makePrisms ''InputArgument)

$(makeLenses ''FunctionPermissionInfo)

$(makeLenses ''FunctionInfo)
