module Main (main) where

import ArrayRelationshipsSpec qualified
import BasicFieldsSpec qualified
import DirectivesSpec qualified
import LimitOffsetSpec qualified
import ObjectRelationshipsSpec qualified
import OrderingSpec qualified
import ServiceLivenessSpec qualified
import Test.Hspec
import ViewsSpec qualified
import WhereSpec qualified
import Prelude

main :: IO ()
main =
  hspec $ do
    describe "ServiceLiveness" ServiceLivenessSpec.spec
    describe "BasicFields" BasicFieldsSpec.spec
    describe "Ordering" OrderingSpec.spec
    describe "Where" WhereSpec.spec
    describe "LimitOffset" LimitOffsetSpec.spec
    describe "ObjectRelationships" ObjectRelationshipsSpec.spec
    describe "ArrayRelationships" ArrayRelationshipsSpec.spec
    describe "Directives" DirectivesSpec.spec
    describe "Views" ViewsSpec.spec
