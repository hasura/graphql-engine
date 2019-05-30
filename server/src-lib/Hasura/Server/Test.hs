module Hasura.Server.Test where

import           Data.Aeson
import qualified Data.Text                  as T
import qualified Database.PG.Query          as Q
import           Debug.Trace
import           Hasura.Prelude
import           Hasura.SQL.Types
import           Language.Haskell.TH.Syntax (Lift)

newtype NEText = NEText {unNEText :: T.Text}
  deriving (Show, Eq, Ord, Hashable, ToJSON, Lift, Q.ToPrepArg, DQuote)

mkNEText :: T.Text -> Maybe NEText
mkNEText ""   = Nothing
mkNEText text = Just $ NEText text

instance FromJSON NEText where
  parseJSON = do
    traceM $! "here"
    withText "String" $ \text -> case mkNEText text of
      Nothing     -> fail "empty string not allowed"
      Just neText -> return neText

instance FromJSONKey NEText where
  fromJSONKey =
    FromJSONKeyValue $ \v -> do
      traceM $! "in instance"
      parseJSON $! v

instance Q.FromCol NEText where
  fromCol bs = mkNEText <$> Q.fromCol bs
    >>= maybe (Left "empty string not allowed") Right
