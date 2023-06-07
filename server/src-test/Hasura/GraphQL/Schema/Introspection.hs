-- | This module contains functions to help with making assertions on the result
-- of parser introspection queries.
module Hasura.GraphQL.Schema.Introspection
  ( queryInputFieldsParserIntrospection,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Ordered qualified as JO
import Data.Text qualified as T
import Hasura.Backends.Postgres.Instances.Schema ()
import Hasura.Base.ErrorMessage
import Hasura.Base.ToErrorValue (ToErrorValue (toErrorValue))
import Hasura.GraphQL.Parser.Name qualified as GName
import Hasura.GraphQL.Schema.Introspect qualified as I
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as G
import Test.Parser.Monad

-- | Produce an introspection parser for an 'InputFieldsParser'.
-- Use the "Test.Parser.Field.field" quasi-quoter to construct the introspection query.
queryInputFieldsParserIntrospection ::
  forall n a.
  -- | The Parser to introspect
  P.InputFieldsParser n a ->
  -- | The Introspection query
  G.Field G.NoFragments P.Variable ->
  IO J.Value
queryInputFieldsParserIntrospection parser field = do
  introspectionParser <- introspectDefintions (P.ifDefinitions parser)
  runParserTest $ P.fParser introspectionParser field

introspectDefintions ::
  forall n a.
  (P.HasTypeDefinitions a, P.MonadParse n) =>
  a ->
  IO (P.FieldParser n J.Value)
introspectDefintions definitions = do
  let introParser :: Either P.ConflictingDefinitions (P.FieldParser n J.Value) = do
        types <- P.collectTypeDefinitions [P.TypeDefinitionsWrapper definitions]
        let schema =
              P.Schema
                { sDescription = Nothing,
                  sTypes = types,
                  sQueryType =
                    P.TNamed
                      P.NonNullable
                      $ P.Definition GName._String Nothing Nothing [] (P.TIObject (P.ObjectInfo [] [])),
                  sMutationType = Nothing,
                  sSubscriptionType = Nothing,
                  sDirectives = []
                }
        return $ (JO.fromOrdered . ($ schema)) <$> I.schema @n

  onLeft introParser (error . T.unpack . fromErrorMessage . toErrorValue)
