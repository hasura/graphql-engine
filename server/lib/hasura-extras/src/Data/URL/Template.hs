-- | A simple URL templating that enables interpolating environment variables
module Data.URL.Template
  ( Template (..),
    TemplateItem,
    Variable,
    printTemplate,
    mkPlainTemplate,
    parseTemplate,
    renderTemplate,
  )
where

import Autodocodec (HasCodec)
import Autodocodec qualified as AC
import Data.Aeson
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import Data.Environment qualified as Env
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Prelude
import Test.QuickCheck

newtype Variable = Variable {unVariable :: Text}
  deriving (Show, Eq, Generic, Hashable)

printVariable :: Variable -> Text
printVariable var = "{{" <> unVariable var <> "}}"

data TemplateItem
  = TIText !Text
  | TIVariable !Variable
  deriving (Show, Eq, Generic)

instance Hashable TemplateItem

printTemplateItem :: TemplateItem -> Text
printTemplateItem = \case
  TIText t -> t
  TIVariable v -> printVariable v

-- | A String with environment variables enclosed in '{{' and '}}'
-- http://{{APP_HOST}}:{{APP_PORT}}/v1/api
newtype Template = Template {unTemplate :: [TemplateItem]}
  deriving (Show, Eq, Generic, Hashable)

instance ToJSON Template where
  toJSON = String . printTemplate

instance FromJSON Template where
  parseJSON = withText "Template" $ \t ->
    onLeft
      (parseTemplate t)
      (\err -> fail $ "Parsing URL template failed: " ++ err)

instance HasCodec Template where
  codec =
    AC.bimapCodec
      (mapLeft ("Parsing URL template failed: " ++) . parseTemplate)
      printTemplate
      AC.codec

printTemplate :: Template -> Text
printTemplate = T.concat . map printTemplateItem . unTemplate

mkPlainTemplate :: Text -> Template
mkPlainTemplate =
  Template . pure . TIText

parseTemplate :: Text -> Either String Template
parseTemplate t = parseOnly parseTemplate' t
  where
    parseTemplate' :: Parser Template
    parseTemplate' = do
      items <- many parseTemplateItem
      lastItem <- TIText <$> takeText
      pure $ Template $ items <> [lastItem]

    parseTemplateItem :: Parser TemplateItem
    parseTemplateItem =
      (TIVariable <$> parseVariable)
        <|> (TIText . T.pack <$> manyTill anyChar (lookAhead $ string "{{"))

    parseVariable :: Parser Variable
    parseVariable =
      string "{{" *> (Variable . T.pack <$> manyTill anyChar (string "}}"))

renderTemplate :: Env.Environment -> Template -> Either Text Text
renderTemplate env template =
  case errorVariables of
    [] -> Right $ T.concat $ rights eitherResults
    _ -> Left (commaSeparated errorVariables)
  where
    eitherResults = map renderTemplateItem $ unTemplate template
    errorVariables = lefts eitherResults
    renderTemplateItem = \case
      TIText t -> Right t
      TIVariable (Variable var) ->
        let maybeEnvValue = Env.lookupEnv env $ T.unpack var
         in case maybeEnvValue of
              Nothing -> Left var
              Just value -> Right $ T.pack value

-- QuickCheck generators
instance Arbitrary Variable where
  arbitrary = Variable . T.pack <$> listOf1 (elements $ alphaNumerics <> " -_")

instance Arbitrary Template where
  arbitrary = Template <$> listOf (oneof [genText, genVariable])
    where
      genText = TIText . T.pack <$> listOf1 (elements $ alphaNumerics <> " ://")
      genVariable = TIVariable <$> arbitrary
