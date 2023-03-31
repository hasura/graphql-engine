-- | Schema parsers for custom return types
module Hasura.CustomReturnType.Schema (buildCustomReturnType) where

import Hasura.CustomReturnType.Cache (CustomReturnTypeInfo (..))
import Hasura.CustomReturnType.IR (CustomReturnType (..))

buildCustomReturnType :: CustomReturnTypeInfo b -> CustomReturnType b
buildCustomReturnType (CustomReturnTypeInfo {_ctiName, _ctiFields}) =
  CustomReturnType
    { crtName = _ctiName,
      crtFields = _ctiFields
    }
