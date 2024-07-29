{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.Postgres.Execute.PrepareSpec
  ( spec,
  )
where

import Data.Aeson.Extended qualified as J (encodeToStrictText)
import Data.HashMap.Strict qualified as HashMap
import Data.Text.NonEmpty (mkNonEmptyTextUnsafe)
import Hasura.Authentication.Role (mkRoleNameSafe)
import Hasura.Authentication.Session (sessionVariablesFromMap, unsafeMkSessionVariable)
import Hasura.Authentication.User (BackendOnlyFieldAccess (..), UserInfo (..))
import Hasura.Backends.Postgres.Execute.Prepare
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types (PGScalarType (..))
import Hasura.Backends.Postgres.SQL.Value
import Hasura.Base.Error (QErr)
import Hasura.Base.Error.TestInstances ()
import Hasura.GraphQL.Parser.Variable (VariableInfo (..))
import Hasura.Prelude
import Hasura.RQL.IR.Value (Provenance (..), UnpreparedValue (..))
import Hasura.RQL.Types.BackendType (BackendType (..), PostgresKind (..))
import Hasura.RQL.Types.Column (ColumnType (..), ColumnValue (..))
import Hasura.SQL.Types (CollectableType (..))
import Language.GraphQL.Draft.Syntax.QQ qualified as G
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

newtype Test x = Test (StateT PlanningSt (Either QErr) x)
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadState PlanningSt, MonadError QErr)

yields :: (Eq x, Show x) => Test x -> x -> Expectation
yields (Test program) answer = evalStateT program initPlanningSt `shouldBe` Right answer

spec :: Spec
spec = do
  let role = mkRoleNameSafe $ mkNonEmptyTextUnsafe "admin"
      userInfo =
        UserInfo
          { _uiRole = role,
            _uiBackendOnlyFieldAccess = BOFAAllowed,
            _uiSession =
              sessionVariablesFromMap
                $ HashMap.fromList
                  [ ("x-hasura-foo", "123"),
                    ("x-hasura-bar", "string_two")
                  ]
          }

  describe "UVSession" do
    describe "prepareWithPlan" do
      it "returns the correct session variable" do
        prepareWithPlan userInfo UVSession `yields` S.SEPrep 1

    describe "prepareWithoutPlan" do
      it "returns the session variable" do
        prepareWithoutPlan userInfo UVSession
          `yields` S.SELit (J.encodeToStrictText (_uiSession userInfo))

  describe "UVLiteral" do
    describe "prepareWithPlan / prepareWithoutPlan" do
      it "handles literals in the same way" do
        let examples = [S.SEPrep 1, S.SENull, S.SELit "hello"]

        for_ examples \x -> do
          prepareWithPlan userInfo (UVLiteral x) `yields` x
          prepareWithoutPlan userInfo (UVLiteral x) `yields` x

  describe "UVParameter" do
    let vi = VIRequired [G.name|foo|]
    let cv = ColumnValue (ColumnScalar @('Postgres 'Vanilla) PGInteger) (PGValInteger 3)

    describe "prepareWithPlan" do
      it "returns the indexed paramemter for PGArray" do
        let cvArray = ColumnValue (ColumnScalar $ PGArray PGInteger) (PGValArray [PGValInteger 1])
        prepareWithPlan userInfo (UVParameter (FromGraphQL vi) cvArray)
          `yields` S.SETyAnn (S.SEPrep 2) (S.TypeAnn "integer[]")

      it "returns the indexed parameter for PGInteger" do
        -- The variable becomes prepared var (2) because that is the first
        -- available parameter index. See 'getVarArgNum'.
        prepareWithPlan userInfo (UVParameter (FromGraphQL vi) cv)
          `yields` S.SETyAnn (S.SEPrep 2) (S.TypeAnn "integer")

      it "reuses variable bindings" do
        let program = do
              x <- prepareWithPlan userInfo (UVParameter (FromGraphQL vi) cv)
              y <- prepareWithPlan userInfo (UVParameter (FromGraphQL vi) cv)

              pure (x, y)

        program
          `yields` ( S.SETyAnn (S.SEPrep 2) (S.TypeAnn "integer"),
                     S.SETyAnn (S.SEPrep 2) (S.TypeAnn "integer")
                   )

      it "reuses internal variable bindings" do
        let program = do
              x <- prepareWithPlan userInfo (UVParameter (FromInternal "foo") cv)
              y <- prepareWithPlan userInfo (UVParameter (FromInternal "bar") cv)
              z <- prepareWithPlan userInfo (UVParameter (FromInternal "foo") cv)

              pure (x, y, z)

        program
          `yields` ( S.SETyAnn (S.SEPrep 2) (S.TypeAnn "integer"),
                     S.SETyAnn (S.SEPrep 3) (S.TypeAnn "integer"),
                     S.SETyAnn (S.SEPrep 2) (S.TypeAnn "integer")
                   )

      it "keeps internal and external bindings separate" do
        let program = do
              x <- prepareWithPlan userInfo (UVParameter (FromInternal "foo") cv)
              y <- prepareWithPlan userInfo (UVParameter (FromGraphQL vi) cv)

              pure (x, y)

        program
          `yields` ( S.SETyAnn (S.SEPrep 2) (S.TypeAnn "integer"),
                     S.SETyAnn (S.SEPrep 3) (S.TypeAnn "integer")
                   )

    describe "prepareWithoutPlan" do
      it "returns the literal value" do
        -- When preparing _without_ a plan, we just inline the value.
        prepareWithoutPlan userInfo (UVParameter (FromGraphQL vi) cv)
          `yields` S.SETyAnn (S.SELit "3") (S.TypeAnn "integer")

  describe "UVSessionVar" do
    let sv = UVSessionVar (CollectableTypeScalar PGInteger) (unsafeMkSessionVariable ("x-hasura-foo" :: Text))

    describe "prepareWithPlan" do
      it "prepares the session variable and accessor" do
        prepareWithPlan userInfo sv
          `yields` S.SETyAnn (S.SEOpApp (S.SQLOp "->>") [S.SEPrep 1, S.SELit "x-hasura-foo"]) (S.TypeAnn "integer")

    describe "prepareWithoutPlan" do
      it "inlines the result" do
        prepareWithoutPlan userInfo sv
          `yields` S.SETyAnn (S.SELit "123") (S.TypeAnn "integer")
