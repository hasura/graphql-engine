module Hasura.RQL.DDL.Headers where

import           Data.Aeson
import           Hasura.Prelude
import           Hasura.RQL.Instances       ()
import           Hasura.RQL.Types.Error
import           Language.Haskell.TH.Syntax (Lift)
import           System.Environment         (lookupEnv)

import qualified Data.Text                  as T


data HeaderConf = HeaderConf HeaderName HeaderValue
   deriving (Show, Eq, Lift, Generic)

instance Hashable HeaderConf

type HeaderName  = T.Text

data HeaderValue = HVValue T.Text | HVEnv T.Text
   deriving (Show, Eq, Lift, Generic)

instance Hashable HeaderValue

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
  toJSON (HeaderConf name (HVEnv val)) = object ["name" .= name, "value_from_env" .= val]


-- | This is used by schema stitching
getHeadersFromConf :: (MonadError QErr m, MonadIO m) => [HeaderConf] -> m [(HeaderName, T.Text)]
getHeadersFromConf = mapM getHeader
  where
    getHeader :: (QErrM m, MonadIO m) => HeaderConf -> m (HeaderName, T.Text)
    getHeader hconf = case hconf of
      (HeaderConf name (HVValue val)) -> return (name, val)
      (HeaderConf name (HVEnv val))   -> do
        mEnv <- liftIO $ lookupEnv (T.unpack val)
        case mEnv of
          Nothing -> throw400 NotFound $ "environment variable '" <> val <> "' not set"
          Just envval -> return (name, T.pack envval)
