module Hasura.RQL.DDL.Headers where

import           Data.Aeson
import           Hasura.Incremental         (Cacheable)
import           Hasura.Prelude
import           Hasura.RQL.Instances       ()
import           Hasura.RQL.Types.Error
import           Language.Haskell.TH.Syntax (Lift)
import           System.Environment         (lookupEnv)

import qualified Data.CaseInsensitive       as CI
import qualified Data.Text                  as T
import qualified Network.HTTP.Types         as HTTP


data HeaderConf = HeaderConf HeaderName HeaderValue
   deriving (Show, Eq, Lift, Generic)
instance NFData HeaderConf
instance Hashable HeaderConf
instance Cacheable HeaderConf

type HeaderName  = T.Text

data HeaderValue = HVValue T.Text | HVEnv T.Text
   deriving (Show, Eq, Lift, Generic)
instance NFData HeaderValue
instance Hashable HeaderValue
instance Cacheable HeaderValue

instance FromJSON HeaderConf where
  parseJSON (Object o) = do
    name <- o .: "name"
    value <- o .:? "value"
    valueFromEnv <- o .:? "value_from_env"
    case (value, valueFromEnv ) of
      (Nothing, Nothing)  -> fail "expecting value or value_from_env keys"
      (Just val, Nothing) -> return $ HeaderConf name (HVValue val)
      (Nothing, Just val) -> return $ HeaderConf name (HVEnv val)
      (Just _, Just _)    -> fail "expecting only one of value or value_from_env keys"
  parseJSON _ = fail "expecting object for headers"

instance ToJSON HeaderConf where
  toJSON (HeaderConf name (HVValue val)) = object ["name" .= name, "value" .= val]
  toJSON (HeaderConf name (HVEnv val))   = object ["name" .= name, "value_from_env" .= val]


-- | Resolve configuration headers
makeHeadersFromConf
  :: (MonadError QErr m, MonadIO m) => [HeaderConf] -> m [HTTP.Header]
makeHeadersFromConf = mapM getHeader
  where
    getHeader hconf =
      ((CI.mk . txtToBs) *** txtToBs) <$>
        case hconf of
          (HeaderConf name (HVValue val)) -> return (name, val)
          (HeaderConf name (HVEnv val))   -> do
            mEnv <- liftIO $ lookupEnv (T.unpack val)
            case mEnv of
              Nothing     -> throw400 NotFound $ "environment variable '" <> val <> "' not set"
              Just envval -> pure (name, T.pack envval)

-- | Encode headers to HeaderConf
toHeadersConf :: [HTTP.Header] -> [HeaderConf]
toHeadersConf =
  map (uncurry HeaderConf . ((bsToTxt . CI.original) *** (HVValue . bsToTxt)))
