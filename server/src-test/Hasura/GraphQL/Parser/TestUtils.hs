module Hasura.GraphQL.Parser.TestUtils where

import           Hasura.Prelude

import qualified Data.HashMap.Strict               as M
import qualified Data.List.NonEmpty                as NE
import qualified Data.Text                         as T
import qualified Language.GraphQL.Draft.Syntax     as G

import           Hasura.GraphQL.Parser.Class.Parse
import           Hasura.GraphQL.Parser.Schema


-- test monad

newtype TestMonad a = TestMonad { runTest :: Either Text a }
  deriving (Functor, Applicative, Monad)

instance MonadParse TestMonad where
  withPath = const id
  parseErrorWith = const $ TestMonad . Left
  markNotReusable = pure ()


-- values generation

fakeScalar :: G.Name -> G.Value Variable
fakeScalar = G.unName >>> \case
  "Int"     -> G.VInt 4242
  "Boolean" -> G.VBoolean False
  name      -> error $ "no test value implemented for scalar " <> T.unpack name

fakeInputFieldValue :: InputFieldInfo -> G.Value Variable
fakeInputFieldValue = \case
  IFOptional t _ -> fromT t
  IFRequired nnt -> fromNNT nnt
  where
    fromT :: forall k. ('Input <: k) => Type k -> G.Value Variable
    fromT = \case
      NonNullable nnt -> fromNNT nnt
      Nullable    nnt -> fromNNT nnt
    fromNNT :: forall k. ('Input <: k) => NonNullableType k -> G.Value Variable
    fromNNT = \case
      TList t -> G.VList [fromT t, fromT t]
      TNamed (Definition name _ _ info) -> case info of
        TIScalar -> fakeScalar name
        TIEnum ei -> G.VEnum $ G.EnumValue $ dName $ NE.head ei
        TIInputObject (InputObjectInfo oi) -> G.VObject $ M.fromList $ do
          Definition fieldName _ _ fieldInfo <- oi
          pure (fieldName, fakeInputFieldValue fieldInfo)
        _ -> error "impossible"

fakeDirective :: DirectiveInfo -> G.Directive Variable
fakeDirective DirectiveInfo{..} =
  G.Directive diName $ M.fromList $ diArguments <&> \(Definition argName _ _ argInfo) ->
    (argName, fakeInputFieldValue argInfo)
