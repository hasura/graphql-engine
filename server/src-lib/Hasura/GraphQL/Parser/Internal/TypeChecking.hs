-- | Here are all functions related to type-checking within the schema. This
-- includes functions that deal with variables, and functions that craft error
-- messages.

module Hasura.GraphQL.Parser.Internal.TypeChecking where

import           Hasura.Prelude

import qualified Data.Aeson                        as A

import           Data.Text.Extended
import           Language.GraphQL.Draft.Syntax     hiding (Definition)

import           Hasura.GraphQL.Parser.Class.Parse
import           Hasura.GraphQL.Parser.Schema


-- | Peeling a variable.
--
-- Whenever we need to inspect a value, for instance when parsing the value of
-- an input argument, there are two main cases: either we have a GraphQL literal
-- (be it explicitly given, or a default argument), or we have a variable:
--
--     where(limit: 42)
--     where(limit: $limit)
--
-- In the case of a variable, we need to "open" it and inspect its content. This
-- forces us to do several things:
--
--   1. Type-checking
--      We know what the variable was declared as, we know what type we expect
--      at our current location in the schema: we can check that they match.
--
--   2. Update re-usability
--      In the past, we used to cache the generated execution plan for a given
--      query. To do so properly, we had to identify queries that couldn't be
--      cached; and any root field that uses the content of a variable (outside
--      of a column's value) couldn't be cached: the same graphql expression
--      would lead to a different execution plan if the value of the variable
--      were to change.  We no longer cache execution plans; but we might do it
--      again in the future, which is why we haven't removed some of the code
--      that deals with re-usability.

peelVariable :: MonadParse m => GType -> InputValue Variable -> m (InputValue Variable)
peelVariable = peelVariableWith False

peelVariableWith :: MonadParse m => Bool -> GType -> InputValue Variable -> m (InputValue Variable)
peelVariableWith locationHasDefaultValue locationType = \case
  GraphQLValue (VVariable var) -> do
    typeCheck locationHasDefaultValue locationType var
    markNotReusable
    pure $ absurd <$> vValue var
  value -> pure value


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
-- and optionality (see Note [Optional fields and nullability] for more
-- details).

typeCheck :: MonadParse m => Bool -> GType -> Variable -> m ()
typeCheck locationHasDefaultValue locationType variable@Variable { vInfo, vType } =
  unless (isVariableUsageAllowed locationHasDefaultValue locationType variable)
    $ parseError
    $ "variable " <> dquote (getName vInfo) <> " is declared as "
      <> showGT vType <> ", but used where "
      <> showGT locationType <> " is expected"

isVariableUsageAllowed
  :: Bool      -- ^ does the location have a default value
  -> GType     -- ^ the location type
  -> Variable  -- ^ the variable
  -> Bool
isVariableUsageAllowed locationHasDefaultValue locationType variable
  | isNullable locationType       = areTypesCompatible locationType variableType
  | not $ isNullable variableType = areTypesCompatible locationType variableType
  | locationHasDefaultValue       = areTypesCompatible locationType variableType
  | hasNonNullDefault variable    = areTypesCompatible locationType variableType
  | otherwise                     = False
  where
    areTypesCompatible = compareTypes `on` \case
      TypeNamed _ n -> TypeNamed (Nullability True) n
      TypeList  _ n -> TypeList  (Nullability True) n
    variableType = vType variable
    hasNonNullDefault = vInfo >>> \case
      VIRequired _       -> False
      VIOptional _ value -> value /= VNull
    compareTypes = curry \case
      (TypeList lNull lType, TypeList vNull vType)
        -> checkNull lNull vNull && areTypesCompatible lType vType
      (TypeNamed lNull lType, TypeNamed vNull vType)
        -> checkNull lNull vNull && lType == vType
      _ -> False
    checkNull (Nullability expectedNull) (Nullability actualNull) =
      expectedNull || not actualNull


-- Error handling functions

typeMismatch :: MonadParse m => Name -> Text -> InputValue Variable -> m a
typeMismatch name expected given = parseError $
  "expected " <> expected <> " for type " <> name <<> ", but found " <> describeValue given

describeValue :: InputValue Variable -> Text
describeValue = describeValueWith (describeValueWith absurd . vValue)

describeValueWith :: (var -> Text) -> InputValue var -> Text
describeValueWith describeVariable = \case
  JSONValue    jval -> describeJSON    jval
  GraphQLValue gval -> describeGraphQL gval
  where
    describeJSON = \case
      A.Null     -> "null"
      A.Bool _   -> "a boolean"
      A.String _ -> "a string"
      A.Number _ -> "a number"
      A.Array  _ -> "a list"
      A.Object _ -> "an object"
    describeGraphQL = \case
      VVariable var -> describeVariable var
      VInt _        -> "an integer"
      VFloat _      -> "a float"
      VString _     -> "a string"
      VBoolean _    -> "a boolean"
      VNull         -> "null"
      VEnum _       -> "an enum value"
      VList _       -> "a list"
      VObject _     -> "an object"
