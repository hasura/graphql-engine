-- | Simple URL templating language enables interpolating environment variables

module Data.URL.Template
  ( URLTemplate
  , Variable
  , printURLTemplate
  , parseURLTemplate
  , renderURLTemplate
  )
where

import           Hasura.Prelude

import qualified Data.Text                  as T

import           Data.Attoparsec.Text
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)
import           System.Environment         (lookupEnv)

newtype Variable = Variable {unVariable :: Text}
  deriving (Show, Eq, Lift, Generic)

-- | A String with single environment variable enclosed in '{{' and '}}'
-- http://{{APP_HOST}}/v1/api
data URLTemplate
  = URLTemplate
  { _utPreVarText  :: !Text
  , _utVariable    :: !Variable
  , _utPostVarText :: !Text
  } deriving (Show, Eq, Lift, Generic)

printURLTemplate :: URLTemplate -> Text
printURLTemplate (URLTemplate preVar var postVar) =
  preVar <> "{{" <> unVariable var <> "}}" <> postVar

parseURLTemplate :: Text -> Either String URLTemplate
parseURLTemplate = parseOnly parseTemplate
  where
    parseTemplate :: Parser URLTemplate
    parseTemplate = URLTemplate
                    <$> (T.pack <$> manyTill anyChar (string "{{"))
                    <*> (Variable . T.pack <$> manyTill anyChar (string "}}"))
                    <*> takeText

renderURLTemplate :: MonadIO m => URLTemplate -> m (Either String Text)
renderURLTemplate (URLTemplate preVar var postVar) = do
  maybeEnvValue <- liftIO $ lookupEnv variableString
  pure $ case maybeEnvValue of
    Nothing    -> Left $ "Value for environment variable " <> variableString <> " not found"
    Just value -> Right $ preVar <> T.pack value <> postVar
  where
    variableString = T.unpack $ unVariable var
