{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module defines all missing instances of third party libraries.
module Hasura.Base.Instances () where

import Control.Monad.Fix
import Data.Aeson qualified as J
import Data.Functor.Product (Product (Pair))
import "some" Data.GADT.Compare (GCompare (gcompare), GOrdering (GEQ, GGT, GLT))
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.OpenApi.Declare as D
import Data.Text qualified as T
import Data.URL.Template qualified as UT
import Database.PG.Query qualified as Q
import Hasura.Prelude
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

instance NFData UT.URLTemplate

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

instance (TH.Lift k, TH.Lift v) => TH.Lift (M.HashMap k v) where
  lift m = [|M.fromList $(TH.lift $ M.toList m)|]
  liftTyped = TH.unsafeTExpCoerce . TH.lift

instance TH.Lift a => TH.Lift (S.HashSet a) where
  lift s = [|S.fromList $(TH.lift $ S.toList s)|]
  liftTyped = TH.unsafeTExpCoerce . TH.lift

deriving instance TH.Lift TDFA.CompOption

deriving instance TH.Lift TDFA.DoPa

deriving instance TH.Lift TDFA.ExecOption

deriving instance TH.Lift TDFA.Pattern

deriving instance TH.Lift TDFA.PatternSet

deriving instance TH.Lift TDFA.PatternSetCharacterClass

deriving instance TH.Lift TDFA.PatternSetCollatingElement

deriving instance TH.Lift TDFA.PatternSetEquivalenceClass

--------------------------------------------------------------------------------
-- GADT

instance (GCompare f, GCompare g) => GCompare (Product f g) where
  Pair a1 a2 `gcompare` Pair b1 b2 = case gcompare a1 b1 of
    GLT -> GLT
    GEQ -> case gcompare a2 b2 of
      GLT -> GLT
      GEQ -> GEQ
      GGT -> GGT
    GGT -> GGT

--------------------------------------------------------------------------------
-- JSON

instance J.FromJSON C.CronSchedule where
  parseJSON = J.withText "CronSchedule" $ \t ->
    onLeft (C.parseCronSchedule t) fail

instance J.ToJSON C.CronSchedule where
  toJSON = J.String . C.serializeCronSchedule

instance J.ToJSONKey Void

--------------------------------------------------------------------------------
-- Postgres

instance Q.ToPrepArg C.CronSchedule where
  toPrepVal = Q.toPrepVal . C.serializeCronSchedule

instance Q.FromCol C.CronSchedule where
  fromCol bs =
    case Q.fromCol bs of
      Left err -> Left err
      Right dbCron ->
        case C.parseCronSchedule dbCron of
          Left err' -> Left $ "invalid cron schedule " <> T.pack err'
          Right cron -> Right cron
