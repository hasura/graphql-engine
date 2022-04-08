module Hasura.Backends.DataWrapper.RQLGenerator.GenAnnSelectG
  ( genAnnSelectG,
  )
where

import Hasura.Backends.DataWrapper.RQLGenerator.GenSelectArgsG (genSelectArgsG)
import Hasura.Backends.DataWrapper.RQLGenerator.GenSelectFromG (genSelectFromG)
import Hasura.Backends.DataWrapper.RQLGenerator.GenTablePermG (genTablePermG)
import Hasura.Generator.Common (defaultRange)
import Hasura.Prelude (Applicative ((<*>)), Bool (..), (<$>), (<&>))
import Hasura.RQL.IR (AnnSelectG (..))
import Hasura.RQL.IR.Generator (genFields)
import Hasura.RQL.Types (BackendType (DataWrapper), StringifyNumbers (..))
import Hedgehog (MonadGen)
import Hedgehog.Gen (bool)

--------------------------------------------------------------------------------

genAnnSelectG :: forall m f a. MonadGen m => m a -> m (f a) -> m (AnnSelectG 'DataWrapper f a)
genAnnSelectG genA genFA =
  AnnSelectG
    <$> genFields genFA defaultRange defaultRange
    <*> genSelectFromG genA
    <*> genTablePermG genA
    <*> genArgs
    <*> genStringifyNumbers
  where
    genStringifyNumbers =
      bool <&> \case
        False -> LeaveNumbersAlone
        True -> StringifyNumbers
    genArgs = genSelectArgsG genA
