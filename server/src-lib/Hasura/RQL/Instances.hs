{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.RQL.Instances where

import           Hasura.Prelude

import qualified Data.Aeson                    as J
import qualified Data.HashMap.Strict           as M
import qualified Data.HashSet                  as S
import qualified Data.URL.Template             as UT
import qualified Language.GraphQL.Draft.Syntax as G
import qualified Language.Haskell.TH.Syntax    as TH
import qualified Text.Regex.TDFA               as TDFA
import qualified Text.Regex.TDFA.Pattern       as TDFA
import qualified Database.PG.Query             as Q

import           Data.Functor.Product
import           Data.GADT.Compare
import           Instances.TH.Lift             ()
import           System.Cron.Parser
import           System.Cron.Types
import           Data.Text

instance NFData G.Argument
instance NFData G.Directive
instance NFData G.ExecutableDefinition
instance NFData G.Field
instance NFData G.FragmentDefinition
instance NFData G.FragmentSpread
instance NFData G.GType
instance NFData G.InlineFragment
instance NFData G.OperationDefinition
instance NFData G.OperationType
instance NFData G.Selection
instance NFData G.TypedOperationDefinition
instance NFData G.Value
instance NFData G.ValueConst
instance NFData G.VariableDefinition
instance (NFData a) => NFData (G.ObjectFieldG a)
instance NFData UT.Variable
instance NFData UT.TemplateItem
instance NFData UT.URLTemplate

deriving instance NFData G.Alias
deriving instance NFData G.EnumValue
deriving instance NFData G.ExecutableDocument
deriving instance NFData G.ListType
deriving instance NFData G.Name
deriving instance NFData G.NamedType
deriving instance NFData G.Nullability
deriving instance NFData G.StringValue
deriving instance NFData G.Variable
deriving instance NFData G.Description
deriving instance (NFData a) => NFData (G.ListValueG a)
deriving instance (NFData a) => NFData (G.ObjectValueG a)

-- instances for CronSchedule from package `cron`
instance NFData StepField
instance NFData RangeField
instance NFData SpecificField
instance NFData BaseField
instance NFData CronField
instance NFData MonthSpec
instance NFData DayOfMonthSpec
instance NFData DayOfWeekSpec
instance NFData HourSpec
instance NFData MinuteSpec
instance NFData CronSchedule

instance (TH.Lift k, TH.Lift v) => TH.Lift (M.HashMap k v) where
  lift m = [| M.fromList $(TH.lift $ M.toList m) |]
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

instance TH.Lift a => TH.Lift (S.HashSet a) where
  lift s = [| S.fromList $(TH.lift $ S.toList s) |]
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

deriving instance TH.Lift TDFA.CompOption
deriving instance TH.Lift TDFA.DoPa
deriving instance TH.Lift TDFA.ExecOption
deriving instance TH.Lift TDFA.Pattern
deriving instance TH.Lift TDFA.PatternSet
deriving instance TH.Lift TDFA.PatternSetCharacterClass
deriving instance TH.Lift TDFA.PatternSetCollatingElement
deriving instance TH.Lift TDFA.PatternSetEquivalenceClass

instance (GEq f, GEq g) => GEq (Product f g) where
  Pair a1 a2 `geq` Pair b1 b2
    | Just Refl <- a1 `geq` b1
    , Just Refl <- a2 `geq` b2
    = Just Refl
    | otherwise = Nothing

instance (GCompare f, GCompare g) => GCompare (Product f g) where
  Pair a1 a2 `gcompare` Pair b1 b2 = case gcompare a1 b1 of
    GLT -> GLT
    GEQ -> case gcompare a2 b2 of
      GLT -> GLT
      GEQ -> GEQ
      GGT -> GGT
    GGT -> GGT

instance J.FromJSON CronSchedule where
  parseJSON = J.withText "CronSchedule" $ \t ->
    either fail pure $ parseCronSchedule t

instance J.ToJSON CronSchedule where
  toJSON = J.String . serializeCronSchedule

instance Q.ToPrepArg CronSchedule where
  toPrepVal = Q.toPrepVal . serializeCronSchedule

instance Q.FromCol CronSchedule where
  fromCol bs =
    case Q.fromCol bs of
      Left err -> Left err
      Right dbCron ->
        case parseCronSchedule dbCron of
          Left err' -> Left $ "invalid cron schedule " <> pack err'
          Right cron -> Right cron
