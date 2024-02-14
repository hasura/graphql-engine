{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module State
  ( tests,
  )
where

import Control.Applicative
import Control.Arrow ((&&&))
import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Metrics.Prometheus.Internal.Map2 as M2
import qualified System.Metrics.Prometheus.Internal.Sample as Sample
import System.Metrics.Prometheus.Internal.State
import Test.Hspec
import qualified Test.Hspec.SmallCheck as SC
import qualified Test.QuickCheck as QC
import qualified Test.SmallCheck as SC
import qualified Test.SmallCheck.Series as SC

------------------------------------------------------------------------

tests :: Spec
tests = do
  stateOpsPreserveInvariants
  registerSmokeTests
  deregisterSmokeTests
  registerGroupSmokeTests
  registerUncheckedDynamicGroupSmokeTests
  deregisterByHandleSmokeTests

------------------------------------------------------------------------

-- * Generating state operations

--
-- We generate "core" state operations on a restricted space of
-- identifiers so that the operations are more likely to interact.

-- ** Restricted inputs

names :: [T.Text]
names = ["a", "b"]

labelSets :: [HashMap.HashMap T.Text T.Text]
labelSets = [HashMap.singleton "k" "v", HashMap.singleton "k" "w"]

identifiers :: [Identifier]
identifiers = Identifier <$> names <*> labelSets

identifier1 :: Identifier
identifier1 = case identifiers of
  (x : _) -> x
  [] -> error "Test implementation error: Not enough identifiers"

identifierGroups :: [[Identifier]]
identifierGroups =
  [ [],
    [a],
    [a, b],
    [a, c],
    [b, c],
    [a, b, c]
  ]
  where
    (a, b, c) = case identifiers of
      (x : y : z : _) -> (x, y, z)
      _ -> error "Test implementation error: Not enough identifiers"

-- Should be related to `defaultSample`
defaultSamplingAction :: MetricSampler
defaultSamplingAction = CounterS (pure 0)

-- Should be related to `defaultSamplingAction`
defaultSample :: Value
defaultSample = Counter 0

defaultGroupSample :: a -> Value
defaultGroupSample = const defaultSample

samplingGroups :: [M.Map Name (Help, M.Map Labels (() -> Value))]
samplingGroups =
  map
    ( Sample.fromList
        . map
          ( \(Identifier name labels) ->
              (name, labels, "", defaultGroupSample)
          )
    )
    identifierGroups

uncheckedSamplingGroups :: [M.Map Name (Help, () -> M.Map Labels Value)]
uncheckedSamplingGroups =
  (map . M.map . fmap) (const . M.map ($ ())) samplingGroups

nonEmptyUncheckedSamplingGroups ::
  [M.Map Name (Help, () -> M.Map Labels Value)]
nonEmptyUncheckedSamplingGroups =
  filter (not . M.null) uncheckedSamplingGroups

-- ** State operation representation

-- | A representation of all state operations, ignoring sampling
-- actions.
data TestStateOp
  = Register Identifier
  | RegisterGroup (M.Map Name (Help, M.Map Labels (() -> Value)))
  | Deregister Identifier
  | RegisterUncheckedDynamicGroup
      (M.Map Name (Help, () -> M.Map Labels Value))

-- | Realize the state operations (using phony sampling actions).
runTestStateOp :: TestStateOp -> State -> State
runTestStateOp op = case op of
  Register identifier ->
    fst . register identifier "" defaultSamplingAction Removable
  RegisterGroup group ->
    fst . registerGroup group (pure ()) Removable
  Deregister identifier ->
    deregister identifier
  RegisterUncheckedDynamicGroup group ->
    fst . registerUncheckedDynamicGroup group (pure ())

instance Show TestStateOp where
  show (Register id') = "Register (" ++ show id' ++ ")"
  show (RegisterGroup idGroup) = "RegisterGroup " ++ show (M.keys idGroup)
  show (Deregister id') = "Deregister (" ++ show id' ++ ")"
  show (RegisterUncheckedDynamicGroup idGroup) =
    "RegisterUncheckedDynamicGroup " ++ show (M.keys idGroup)

instance (Monad m) => SC.Serial m TestStateOp where
  series =
    asum
      [ Register <$> choose identifiers,
        RegisterGroup <$> choose samplingGroups,
        Deregister <$> choose identifiers,
        RegisterUncheckedDynamicGroup <$> choose uncheckedSamplingGroups
      ]
    where
      choose :: (Alternative f) => [a] -> f a
      choose = foldr ((<|>) . pure) empty

instance QC.Arbitrary TestStateOp where
  -- \| Frequencies are biased towards registration but are otherwise
  -- arbitrary
  arbitrary =
    QC.frequency
      [ (4, Register <$> QC.elements identifiers),
        (2, RegisterGroup <$> QC.elements samplingGroups),
        (4, Deregister <$> QC.elements identifiers),
        ( 1,
          RegisterUncheckedDynamicGroup
            <$> QC.elements uncheckedSamplingGroups
        )
      ]

-- ** Random generation of `State`s through operations

runTestStateOps :: [TestStateOp] -> State -> State
runTestStateOps ops state = foldl' (flip runTestStateOp) state ops

-- | Use sequences of state operations to generate random states.
makeStateFromOps :: [TestStateOp] -> State
makeStateFromOps ops = runTestStateOps ops initialState

------------------------------------------------------------------------

-- * Preservation of state invariants by state operations

stateOpsPreserveInvariants :: Spec
stateOpsPreserveInvariants =
  describe "A sequence of operations on the internal state" $ do
    let verifyOps :: [TestStateOp] -> Bool
        verifyOps = verifyState . makeStateFromOps
    it "preserves internal consistency (smallcheck)" $
      -- A depth of 4 yields sequences of operations up to length 3.
      -- The test takes too long if we go any deeper.
      SC.property $
        SC.changeDepth (const 4) $
          SC.forAll verifyOps
    it "preserves internal consistency (quickcheck)" $
      QC.property verifyOps

------------------------------------------------------------------------

-- * Register

registerSmokeTests :: Spec
registerSmokeTests =
  describe "The register operation" $ do
    it "registers the intended metric" $
      QC.property prop_registerRegisters
    it "is idempotent" $
      QC.property prop_registerIdempotent

prop_registerRegisters :: [TestStateOp] -> Bool
prop_registerRegisters ops =
  let state0 = makeStateFromOps ops
      x = 99
      sampler = GaugeS (pure x)
      value = Gauge x
      (state1, _handle) =
        register identifier1 "" sampler Removable state0
      sampledMetrics1 =
        unsafePerformIO $
          sampledStateMetrics <$> sampleState state1
      Identifier name1 labels1 = identifier1
   in case Sample.lookup name1 labels1 sampledMetrics1 of
        Nothing -> False
        Just (e, _) -> case e of
          Right _ -> False
          Left value' -> value == value'

prop_registerIdempotent :: [TestStateOp] -> Bool
prop_registerIdempotent ops =
  let state0 = makeStateFromOps ops
      sampler = GaugeS (pure 99)
      (state1, _handle1) =
        register identifier1 "" sampler Removable state0
      (state2, _handle2) =
        register identifier1 "" sampler Removable state1
      sampledState1 = unsafePerformIO $ sampleState state1
      sampledState2 = unsafePerformIO $ sampleState state2
   in functionallyEqual sampledState1 sampledState2

------------------------------------------------------------------------

-- * Register group

registerGroupSmokeTests :: Spec
registerGroupSmokeTests =
  describe "The registerGroup operation" $ do
    it "registers the intended metrics" $
      QC.property prop_registerGroupRegisters
    it "is idempotent" $
      QC.property prop_registerGroupIdempotent

prop_registerGroupRegisters :: [TestStateOp] -> Bool
prop_registerGroupRegisters ops =
  let state0 = makeStateFromOps ops
      identifiers2 = take 2 identifiers
      (getters, values) =
        unzip $
          map (const . Gauge &&& Gauge) $
            iterate succ 99
      (state1, _handle1) =
        registerGroup
          (makeSampleGroup $ zip identifiers2 getters)
          (pure ())
          Removable
          state0
      sampledState1 = unsafePerformIO $ sampleState state1
      sampledStateMetrics1 = sampledStateMetrics sampledState1
      sampledStateGroups1 = sampledStateGroups sampledState1
   in flip all (zip identifiers2 values) $
        \(Identifier name labels, value) ->
          case Sample.lookup name labels sampledStateMetrics1 of
            Nothing -> False
            Just (e, _) -> case e of
              Left _ -> False
              Right groupId ->
                case M.lookup groupId sampledStateGroups1 of
                  Nothing -> False
                  Just groups -> case M2.lookup name labels groups of
                    Nothing -> False
                    Just value' -> value == value'

prop_registerGroupIdempotent :: [TestStateOp] -> Bool
prop_registerGroupIdempotent ops =
  let state0 = makeStateFromOps ops
      identifiers2 = take 2 identifiers
      getters = map (const . Gauge) $ iterate succ 99
      sampleGroup = makeSampleGroup $ zip identifiers2 getters
      sampler = pure ()
      (state1, _handle1) =
        registerGroup sampleGroup sampler Removable state0
      (state2, _handle2) =
        registerGroup sampleGroup sampler Removable state1
      sampledState1 = unsafePerformIO $ sampleState state1
      sampledState2 = unsafePerformIO $ sampleState state2
   in functionallyEqual sampledState1 sampledState2

makeSampleGroup ::
  [(Identifier, a -> Value)] ->
  M.Map Name (Help, M.Map Labels (a -> Value))
makeSampleGroup xs =
  Sample.fromList $
    flip map xs $
      \(Identifier name labels, getter) ->
        (name, labels, "", getter)

------------------------------------------------------------------------

-- * Deregister

deregisterSmokeTests :: Spec
deregisterSmokeTests = do
  describe "The deregister operation" $ do
    it "deregisters the intended metric" $
      QC.property prop_deregisterDeregisters
    it "is idempotent" $
      QC.property prop_deregisterIdempotent

prop_deregisterDeregisters :: [TestStateOp] -> Bool
prop_deregisterDeregisters ops =
  let state0 =
        runTestStateOps ops $
          fst $
            register
              identifier1
              ""
              (GaugeS (pure 99))
              Removable
              initialState
      state1 = deregister identifier1 state0
      sampledState1 = unsafePerformIO $ sampleState state1
      Identifier name1 labels1 = identifier1
   in not $ Sample.member name1 labels1 $ sampledStateMetrics sampledState1

prop_deregisterIdempotent :: [TestStateOp] -> Bool
prop_deregisterIdempotent ops =
  let state0 =
        runTestStateOps ops $
          fst $
            register
              identifier1
              ""
              (GaugeS (pure 99))
              Removable
              initialState
      state1 = deregister identifier1 state0
      state2 = deregister identifier1 state1
      sampledState1 = unsafePerformIO $ sampleState state1
      sampledState2 = unsafePerformIO $ sampleState state2
   in functionallyEqual sampledState1 sampledState2

------------------------------------------------------------------------

-- * Unchecked dynamic groups

registerUncheckedDynamicGroupSmokeTests :: Spec
registerUncheckedDynamicGroupSmokeTests = do
  describe "Unchecked dynamic groups" $ do
    it "The registerUncheckedDynamicGroup operation works" $
      QC.property prop_registerUncheckedDynamicGroupRegisters
    it "The deregisterUncheckedDynamicGroup operation works" $
      QC.property prop_deregisterUncheckedDynamicGroupDeregisters

prop_registerUncheckedDynamicGroupRegisters :: [TestStateOp] -> Bool
prop_registerUncheckedDynamicGroupRegisters ops =
  let state0 = makeStateFromOps ops
      Identifier name labelSet = head identifiers
      valuesMap = M.singleton labelSet (Gauge 99)
      dynamicGroup = M.singleton name ("", const valuesMap)
      expectedSample = M.singleton name ("", valuesMap)
      (state1, _handle1) =
        registerUncheckedDynamicGroup
          dynamicGroup
          (pure ())
          state0
      sampledState1 = unsafePerformIO $ sampleState state1
   in expectedSample
        `elem` sampledStateUncheckedDynamicGroups sampledState1

prop_deregisterUncheckedDynamicGroupDeregisters :: [TestStateOp] -> Bool
prop_deregisterUncheckedDynamicGroupDeregisters ops =
  let state0 = makeStateFromOps ops
      Identifier name labelSet = head identifiers
      valuesMap = M.singleton labelSet (Gauge 99)
      dynamicGroup = M.singleton name ("", const valuesMap)
      (state1, maybeHandle1) =
        registerUncheckedDynamicGroup
          dynamicGroup
          (pure ())
          state0
      state2 =
        deregisterUncheckedDynamicGroup
          (unsafeGetHandleVersion (fromJust maybeHandle1))
          state1
      sampledState0 = unsafePerformIO $ sampleState state0
      sampledState2 = unsafePerformIO $ sampleState state2
   in functionallyEqual sampledState0 sampledState2

------------------------------------------------------------------------

-- * Deregister by handle

deregisterByHandleSmokeTests :: Spec
deregisterByHandleSmokeTests =
  describe "Deregistration by a handle" $ do
    it "removes the intended metric" $
      QC.property prop_handleDeregisters
    it "removes unchecked dynamic groups" $
      QC.property prop_uncheckedHandleDeregisters
    it "has no effect if the metric is already deregistered" $
      QC.property prop_handleSpecificity

prop_handleDeregisters :: [TestStateOp] -> Bool
prop_handleDeregisters ops =
  let state0 = makeStateFromOps ops
      (state1, handle1) =
        register identifier1 "" (GaugeS (pure 99)) Removable state0
      state2 = deregisterByHandle handle1 state1
      sampledState2 = unsafePerformIO $ sampleState state2
      Identifier name1 labels1 = identifier1
   in not $ Sample.member name1 labels1 $ sampledStateMetrics sampledState2

prop_uncheckedHandleDeregisters :: [TestStateOp] -> Bool
prop_uncheckedHandleDeregisters ops =
  let state0 = makeStateFromOps ops
      (state1, handleMaybe1) =
        registerUncheckedDynamicGroup
          (head nonEmptyUncheckedSamplingGroups)
          (pure ())
          state0
      state2 = deregisterByHandle (fromJust handleMaybe1) state1
      sampledState0 = unsafePerformIO $ sampleState state0
      sampledState2 = unsafePerformIO $ sampleState state2
   in functionallyEqual sampledState0 sampledState2

prop_handleSpecificity :: [TestStateOp] -> Bool
prop_handleSpecificity ops =
  let state0 = makeStateFromOps ops
      (state1, handle) =
        register identifier1 "" (GaugeS (pure 99)) Removable state0
      (state2, _) =
        register identifier1 "" (GaugeS (pure 98)) Removable state1
      state3 = deregisterByHandle handle state2
      sampledState2 = unsafePerformIO $ sampleState state2
      sampledState3 = unsafePerformIO $ sampleState state3
   in sampledState2 == sampledState3
