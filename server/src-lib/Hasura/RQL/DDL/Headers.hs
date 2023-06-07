module Hasura.RQL.DDL.Headers
  ( makeHeadersFromConf,
    toHeadersConf,
  )
where

import Data.CaseInsensitive qualified as CI
import Data.Environment qualified as Env
import Data.Text qualified as T
import Hasura.Base.Error
import Hasura.Base.Instances ()
import Hasura.Prelude
import Hasura.RQL.Types.Headers
import Network.HTTP.Types qualified as HTTP

-- | Resolve configuration headers
makeHeadersFromConf ::
  (MonadError QErr m) => Env.Environment -> [HeaderConf] -> m [HTTP.Header]
makeHeadersFromConf env = mapM getHeader
  where
    getHeader hconf =
      ((CI.mk . txtToBs) *** txtToBs)
        <$> case hconf of
          (HeaderConf name (HVValue val)) -> return (name, val)
          (HeaderConf name (HVEnv val)) -> do
            let mEnv = Env.lookupEnv env (T.unpack val)
            case mEnv of
              Nothing -> throw400 NotFound $ "environment variable '" <> val <> "' not set"
              Just envval -> pure (name, T.pack envval)

-- | Encode headers to HeaderConf
toHeadersConf :: [HTTP.Header] -> [HeaderConf]
toHeadersConf =
  map (uncurry HeaderConf . ((bsToTxt . CI.original) *** (HVValue . bsToTxt)))
