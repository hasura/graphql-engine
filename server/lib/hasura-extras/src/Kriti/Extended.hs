module Kriti.Extended (fieldAccessPathTailValues) where

import Data.Aeson.KeyMap qualified as KeyMap
import Data.List.Extra (unsnoc)
import Data.Vector qualified as Vector
import Hasura.Prelude
import Kriti.Parser qualified as Kriti

-- | Extracts tail values of the given field access path.
-- Traverses the kriti template recursively to match the path with the field accessors.
-- If the path is matched, then the tail value is extracted.
fieldAccessPathTailValues :: [Text] -> Kriti.ValueExt -> [Text]
fieldAccessPathTailValues path = \case
  -- Field accessors
  Kriti.RequiredFieldAccess _span value tailValue -> fieldAccessTail path value tailValue
  Kriti.OptionalFieldAccess _span value tailValues -> concatMap (fieldAccessTail path value) tailValues
  -- We need to recurse into for the following cases
  Kriti.Object _span keyMap -> concatMap (fieldAccessPathTailValues path) $ KeyMap.elems keyMap
  Kriti.Array _span values -> concatMap (fieldAccessPathTailValues path) $ Vector.toList values
  Kriti.StringTem _span values -> concatMap (fieldAccessPathTailValues path) $ Vector.toList values
  Kriti.Iff _span value1 value2 elIfs elseValue ->
    fieldAccessPathTailValues path value1
      <> fieldAccessPathTailValues path value2
      <> concatMap (\(Kriti.Elif _span val1 val2) -> fieldAccessPathTailValues path val1 <> fieldAccessPathTailValues path val2) elIfs
      <> fieldAccessPathTailValues path elseValue
  Kriti.Eq _span value1 value2 ->
    fieldAccessPathTailValues path value1
      <> fieldAccessPathTailValues path value2
  Kriti.NotEq _span value1 value2 ->
    fieldAccessPathTailValues path value1
      <> fieldAccessPathTailValues path value2
  Kriti.Gt _span value1 value2 ->
    fieldAccessPathTailValues path value1
      <> fieldAccessPathTailValues path value2
  Kriti.Gte _span value1 value2 ->
    fieldAccessPathTailValues path value1
      <> fieldAccessPathTailValues path value2
  Kriti.Lt _span value1 value2 ->
    fieldAccessPathTailValues path value1
      <> fieldAccessPathTailValues path value2
  Kriti.Lte _span value1 value2 ->
    fieldAccessPathTailValues path value1
      <> fieldAccessPathTailValues path value2
  Kriti.And _span value1 value2 ->
    fieldAccessPathTailValues path value1
      <> fieldAccessPathTailValues path value2
  Kriti.Or _span value1 value2 ->
    fieldAccessPathTailValues path value1
      <> fieldAccessPathTailValues path value2
  Kriti.In _span value1 value2 ->
    fieldAccessPathTailValues path value1
      <> fieldAccessPathTailValues path value2
  Kriti.Defaulting _span value1 value2 ->
    fieldAccessPathTailValues path value1
      <> fieldAccessPathTailValues path value2
  Kriti.Range _span _ _ value1 value2 ->
    fieldAccessPathTailValues path value1
      <> fieldAccessPathTailValues path value2
  Kriti.Function _span _ value -> fieldAccessPathTailValues path value
  -- We don't need to recurse into for the following cases
  Kriti.String _span _ -> []
  Kriti.Number _span _ -> []
  Kriti.Boolean _span _ -> []
  Kriti.Null _span -> []
  Kriti.Var _span _varName -> []

-- | Match the path with the field accessors in the kriti template
matchPath :: [Text] -> Kriti.ValueExt -> Bool
matchPath path kritiValue =
  case unsnoc path of
    Nothing ->
      -- Received an empty path
      False
    Just ([], firstElem) ->
      -- Reached the initial end of the path
      -- Now, kritiValue should be a variable that matches the firstElem
      case kritiValue of
        Kriti.Var _span varName -> varName == firstElem
        _ -> False
    Just (initPath, tail') ->
      -- We need to recurse into for field access
      case kritiValue of
        Kriti.RequiredFieldAccess _span value tailValue ->
          matchPath initPath value && ((Just tail') == coerceTailValueAsText tailValue)
        _ -> False

-- | Get the tail of the path for a field access
--
--  fieldAccessTail ["a", "b"]
--  match case:
--    $.a.b.c -> ["c"]
--    $.a.b.[c] -> ["c"]
--    $.a.b.['c'] -> ["c"]
--    $.a.b.["c"] -> ["c"]
--    $.a.b.[<kriti_exp>] -> []
--  not match case:
--    $.c.d.[<kriti_exp>] -> fieldAccessPathTailValues ["a", "b"] kriti_exp
fieldAccessTail ::
  [Text] ->
  Kriti.ValueExt ->
  Either Text Kriti.ValueExt ->
  [Text]
fieldAccessTail path accessPath tailValue =
  if matchPath path accessPath
    then maybeToList $ coerceTailValueAsText tailValue
    else -- If path is not matched, look for kriti expressions in the tailValue
    case tailValue of
      Right kritiExp -> fieldAccessPathTailValues path kritiExp
      Left _ -> []

-- | Coerce the tail value of the path as text
--
-- path.tail_value     => Left text
-- path.[tail_value]   => Right (Var _ text)
-- path.['tail_value'] => Right (String _ text)
-- path.["tail_value"] => Right (StringTem _ [String _ text])
coerceTailValueAsText :: Either Text Kriti.ValueExt -> Maybe Text
coerceTailValueAsText = \case
  Left t -> Just t
  Right (Kriti.Var _span t) -> Just t
  Right (Kriti.String _span t) -> Just t
  Right (Kriti.StringTem _span templates) ->
    case Vector.toList templates of
      [Kriti.String _span' t] -> Just t
      _ -> Nothing
  Right _ -> Nothing
