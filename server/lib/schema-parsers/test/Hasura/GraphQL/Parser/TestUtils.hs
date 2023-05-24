module Hasura.GraphQL.Parser.TestUtils
  ( TestMonad (..),
    fakeScalar,
    fakeInputFieldValue,
    fakeDirective,
  )
where

import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Hasura.Base.ErrorMessage (ErrorMessage)
import Hasura.GraphQL.Parser
import Hasura.GraphQL.Parser.Name qualified as GName
import Language.GraphQL.Draft.Syntax qualified as G

-- test monad

newtype TestMonad a = TestMonad {runTest :: Either ErrorMessage a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadParse TestMonad where
  withKey = const id
  parseErrorWith = const $ TestMonad . Left

-- values generation

fakeScalar :: G.Name -> G.Value Variable
fakeScalar name =
  if
    | name == GName._Int -> G.VInt 4242
    | name == GName._Boolean -> G.VBoolean False
    | otherwise -> error $ "no test value implemented for scalar " <> show name

fakeInputFieldValue :: forall origin. InputFieldInfo origin -> G.Value Variable
fakeInputFieldValue (InputFieldInfo t _) = go t
  where
    go :: forall k. ('Input <: k) => Type origin k -> G.Value Variable
    go = \case
      TList _ t' -> G.VList [go t', go t']
      TNamed _ (Definition name _ _ _ info) -> case (info, subKind @'Input @k) of
        (TIScalar, _) -> fakeScalar name
        (TIEnum ei, _) -> G.VEnum $ G.EnumValue $ dName $ NE.head ei
        (TIInputObject (InputObjectInfo oi), _) -> G.VObject $
          HashMap.fromList $ do
            Definition fieldName _ _ _ fieldInfo <- oi
            pure (fieldName, fakeInputFieldValue fieldInfo)
        _ -> error "fakeInputFieldValue: non-exhaustive. FIXME"

fakeDirective :: DirectiveInfo origin -> G.Directive Variable
fakeDirective DirectiveInfo {..} =
  G.Directive diName $
    HashMap.fromList $
      diArguments <&> \(Definition argName _ _ _ argInfo) ->
        (argName, fakeInputFieldValue argInfo)
