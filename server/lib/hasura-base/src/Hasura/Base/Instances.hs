{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module defines all missing instances of third party libraries.
module Hasura.Base.Instances () where

import Autodocodec qualified as AC
import Control.Monad.Fix
import Data.Aeson qualified as J
import Data.ByteString (ByteString)
import Data.Fixed (Fixed (..))
import Data.OpenApi.Declare as D
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Extended (ToTxt (toTxt))
import Data.Time (NominalDiffTime)
import Data.URL.Template qualified as UT
import Database.ODBC.SQLServer qualified as ODBC
import Database.PG.Query qualified as PG
import Hasura.Prelude
import Kriti qualified
import Kriti.Parser qualified as Kriti
import Language.Haskell.TH.Lift qualified as TH (deriveLift)
import Language.Haskell.TH.Syntax qualified as TH
import System.Cron.Parser qualified as C
import System.Cron.Types qualified as C
import Text.Regex.TDFA qualified as TDFA
import Text.Regex.TDFA.Pattern qualified as TDFA

--------------------------------------------------------------------------------
-- MonadFix

instance (Monoid d, MonadFix m) => MonadFix (DeclareT d m) where
  mfix f = DeclareT $ \s -> mfix $ \ ~(_, a) -> runDeclareT (f a) s
  {-# INLINE mfix #-}

--------------------------------------------------------------------------------
-- Deepseq

instance NFData UT.Variable

instance NFData UT.TemplateItem

instance NFData UT.Template

instance NFData C.StepField

instance NFData C.RangeField

instance NFData C.SpecificField

instance NFData C.BaseField

instance NFData C.CronField

instance NFData C.MonthSpec

instance NFData C.DayOfMonthSpec

instance NFData C.DayOfWeekSpec

instance NFData C.HourSpec

instance NFData C.MinuteSpec

instance NFData C.CronSchedule

--------------------------------------------------------------------------------
-- Template Haskell

deriving instance TH.Lift TDFA.CompOption

deriving instance TH.Lift TDFA.DoPa

deriving instance TH.Lift TDFA.ExecOption

deriving instance TH.Lift TDFA.Pattern

deriving instance TH.Lift TDFA.PatternSet

deriving instance TH.Lift (Fixed a)

deriving instance TH.Lift TDFA.PatternSetCharacterClass

deriving instance TH.Lift TDFA.PatternSetCollatingElement

deriving instance TH.Lift TDFA.PatternSetEquivalenceClass

$(TH.deriveLift ''DiffTime)

$(TH.deriveLift ''NominalDiffTime)

deriving instance TH.Lift Milliseconds

deriving instance TH.Lift Seconds

--------------------------------------------------------------------------------
-- HasCodec

instance AC.HasCodec C.CronSchedule where
  codec =
    AC.named "CronSchedule"
      $ AC.bimapCodec C.parseCronSchedule C.serializeCronSchedule
      $ AC.codec @Text

--------------------------------------------------------------------------------
-- JSON

instance J.FromJSON C.CronSchedule where
  parseJSON = J.withText "CronSchedule" $ \t ->
    onLeft (C.parseCronSchedule t) fail

instance J.ToJSON C.CronSchedule where
  toJSON = J.String . C.serializeCronSchedule

instance J.FromJSON ByteString where
  parseJSON = J.withText "ByteString" (pure . encodeUtf8)

instance J.ToJSON ByteString where
  toJSON = J.String . decodeUtf8With lenientDecode

--------------------------------------------------------------------------------
-- ODBC

instance ToTxt ODBC.Query where
  toTxt = ODBC.renderQuery

--------------------------------------------------------------------------------
-- Postgres

instance PG.ToPrepArg C.CronSchedule where
  toPrepVal = PG.toPrepVal . C.serializeCronSchedule

instance PG.FromCol C.CronSchedule where
  fromCol bs =
    case PG.fromCol bs of
      Left err -> Left err
      Right dbCron ->
        case C.parseCronSchedule dbCron of
          Left err' -> Left $ "invalid cron schedule " <> T.pack err'
          Right cron -> Right cron

--------------------------------------------------------------------------------
-- Kriti

instance NFData Kriti.AlexSourcePos

instance NFData Kriti.Span

instance NFData Kriti.Elif

instance NFData Kriti.ValueExt
