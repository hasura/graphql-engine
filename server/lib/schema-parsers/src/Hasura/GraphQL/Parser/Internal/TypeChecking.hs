-- | Here are all functions related to type-checking within the schema. This
-- includes functions that deal with variables, and functions that craft error
-- messages.
module Hasura.GraphQL.Parser.Internal.TypeChecking
  ( peelVariable,
    peelVariableWith,
    typeCheck,
    typeMismatch,
  )
where

import Data.Aeson qualified as J
import Hasura.Base.ErrorMessage
import Hasura.Base.ToErrorValue
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Parser.Names
import Hasura.GraphQL.Parser.Variable
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax hiding (Definition)

-- | Peeling a variable.
--
-- Whenever we need to inspect a value, for instance when parsing the value of
-- an input argument, there are two main cases: either we have a GraphQL literal
-- (be it explicitly given, or a default argument), or we have a variable:
--
--     where(limit: 42)
--     where(limit: $limit)
--
-- In the case of a variable, we are now in the context type-check the variable
-- usage: we know what the variable was declared as, and we know what type we
-- expect at our current location in the schema: we can check that they match.
--
-- Note: this conflates nulls and absent variable values (GraphQL spec, June
-- 2018, section 2.9.5), converting the latter into the former.
peelVariable :: (MonadParse m) => GType -> InputValue Variable -> m (InputValue Variable)
peelVariable locationType value =
  peelVariableWith False locationType value
    `onNothingM` if isNullable locationType
      then pure $ GraphQLValue VNull
      else parseError $ "no variable value specified for non-nullable variable " <> describeValue value

peelVariableWith :: (MonadParse m) => Bool -> GType -> InputValue Variable -> m (Maybe (InputValue Variable))
peelVariableWith locationHasDefaultValue locationType = \case
  GraphQLValue (VVariable var) -> do
    typeCheck locationHasDefaultValue locationType var
    pure $ fmap absurd <$> vValue var
  value -> pure $ Just value

-- | Type-checking.
--
-- Checks whether the type of a variable is compatible with the type at the
-- location at which it is used. This is an implementation of the function
-- described in §5.8.5 of the spec:
--     http://spec.graphql.org/June2018/#sec-All-Variable-Usages-are-Allowed
--
-- No input type coercion is allowed between variables: coercion
-- rules only allow when translating a value from a literal. It is
-- therefore not allowed to use an Int variable at a Float location,
-- despite the fact that it is legal to use an Int literal at a
-- Float location.
--
-- Furthermore, it's also worth noting that there's one tricky case where we
-- might allow a nullable variable at a non-nullable location: when either side
-- has a non-null default value. That's because GraphQL conflates nullability
-- and optionality. See also Note [When are fields optional?].
typeCheck :: (MonadParse m) => Bool -> GType -> Variable -> m ()
typeCheck locationHasDefaultValue locationType variable@Variable {vInfo, vType} =
  unless (isVariableUsageAllowed locationHasDefaultValue locationType variable) $
    parseError $
      "variable "
        <> toErrorValue (getName vInfo)
        <> " is declared as "
        <> toErrorValue vType
        <> ", but used where "
        <> toErrorValue locationType
        <> " is expected"

isVariableUsageAllowed ::
  -- | does the location have a default value
  Bool ->
  -- | the location type
  GType ->
  -- | the variable
  Variable ->
  Bool
isVariableUsageAllowed locationHasDefaultValue locationType variable
  | isNullable locationType = areTypesCompatible locationType variableType
  | not $ isNullable variableType = areTypesCompatible locationType variableType
  | locationHasDefaultValue = areTypesCompatible locationType variableType
  | hasNonNullDefault variable = areTypesCompatible locationType variableType
  | otherwise = False
  where
    areTypesCompatible =
      compareTypes `on` \case
        TypeNamed _ n -> TypeNamed (Nullability True) n
        TypeList _ n -> TypeList (Nullability True) n
    variableType = vType variable
    hasNonNullDefault =
      vInfo >>> \case
        VIRequired _ -> False
        VIOptional _ value -> value /= VNull
    compareTypes = curry \case
      (TypeList lNull lType, TypeList vNull vType) ->
        checkNull lNull vNull && areTypesCompatible lType vType
      (TypeNamed lNull lType, TypeNamed vNull vType) ->
        checkNull lNull vNull && lType == vType
      _ -> False
    checkNull (Nullability expectedNull) (Nullability actualNull) =
      expectedNull || not actualNull

-- Error handling functions

typeMismatch :: (HasName n, MonadParse m) => n -> ErrorMessage -> InputValue Variable -> m a
typeMismatch name expected given =
  parseError $
    "expected " <> expected <> " for type " <> toErrorValue (getName name) <> ", but found " <> describeValue given

describeValue :: InputValue Variable -> ErrorMessage
describeValue = describeValueWith describeVariable

describeVariable :: Variable -> ErrorMessage
describeVariable v@Variable {..} = case vValue of
  Nothing -> "no value specified for variable " <> toErrorValue (getName v)
  Just val -> describeValueWith absurd val

describeValueWith :: (var -> ErrorMessage) -> InputValue var -> ErrorMessage
describeValueWith describeVariable' = \case
  JSONValue jval -> describeJSON jval
  GraphQLValue gval -> describeGraphQL gval
  where
    describeJSON = \case
      J.Null -> "null"
      J.Bool _ -> "a boolean"
      J.String _ -> "a string"
      J.Number _ -> "a number"
      J.Array _ -> "a list"
      J.Object _ -> "an object"
    describeGraphQL = \case
      VVariable var -> describeVariable' var
      VInt _ -> "an integer"
      VFloat _ -> "a float"
      VString _ -> "a string"
      VBoolean _ -> "a boolean"
      VNull -> "null"
      VEnum _ -> "an enum value"
      VList _ -> "a list"
      VObject _ -> "an object"
