module Hasura.Backends.DataConnector.RQLGenerator.GenAnnSelectG
  ( genAnnSelectG,
  )
where

import Hasura.Backends.DataConnector.RQLGenerator.GenSelectArgsG (genSelectArgsG)
import Hasura.Backends.DataConnector.RQLGenerator.GenSelectFromG (genSelectFromG)
import Hasura.Backends.DataConnector.RQLGenerator.GenTablePermG (genTablePermG)
import Hasura.Generator.Common (defaultRange)
import Hasura.Prelude (Applicative ((<*>)), Bool (..), (<$>), (<&>))
import Hasura.RQL.IR (AnnSelectG (..))
import Hasura.RQL.IR.Generator (genFields)
import Hasura.RQL.Types.Common (StringifyNumbers (..))
import Hasura.SQL.Backend (BackendType (..))
import Hedgehog (MonadGen)
import Hedgehog.Gen (bool)

--------------------------------------------------------------------------------

genAnnSelectG :: forall m f a. MonadGen m => m a -> m (f a) -> m (AnnSelectG 'DataConnector f a)
genAnnSelectG genA genFA =
  AnnSelectG
    <$> genFields genFA defaultRange defaultRange
    <*> genSelectFromG
    <*> genTablePermG genA
    <*> genArgs
    <*> genStringifyNumbers
  where
    genStringifyNumbers =
      bool <&> \case
        False -> LeaveNumbersAlone
        True -> StringifyNumbers
    genArgs = genSelectArgsG genA
