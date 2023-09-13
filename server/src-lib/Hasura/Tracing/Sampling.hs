module Hasura.Tracing.Sampling
  ( -- * SamplingState
    SamplingState (..),
    samplingStateToHeader,
    samplingStateFromHeader,

    -- * SamplingDecision
    SamplingDecision (..),

    -- * SamplingPolicy
    SamplingPolicy,
    sampleNever,
    sampleAlways,
    sampleRandomly,
    sampleOneInN,
  )
where

import Hasura.Prelude
import Refined (Positive, Refined, unrefine)
import System.Random.Stateful qualified as Random

--------------------------------------------------------------------------------
-- SamplingState

-- | B3 propagation sampling state.
--
-- Debug sampling state not represented.
data SamplingState = SamplingDefer | SamplingDeny | SamplingAccept
  deriving (Show, Eq)

-- | Convert a sampling state to a value for the X-B3-Sampled header. A return
-- value of Nothing indicates that the header should not be set.
samplingStateToHeader :: (IsString s) => SamplingState -> Maybe s
samplingStateToHeader = \case
  SamplingDefer -> Nothing
  SamplingDeny -> Just "0"
  SamplingAccept -> Just "1"

-- | Convert a X-B3-Sampled header value to a sampling state. An input of
-- Nothing indicates that the header was not set.
samplingStateFromHeader :: (IsString s, Eq s) => Maybe s -> SamplingState
samplingStateFromHeader = \case
  Nothing -> SamplingDefer
  Just "0" -> SamplingDeny
  Just "1" -> SamplingAccept
  Just _ -> SamplingDefer

--------------------------------------------------------------------------------
-- SamplingDecision

-- | A local decision about whether or not to sample spans.
data SamplingDecision = SampleNever | SampleAlways

--------------------------------------------------------------------------------
-- SamplingPolicy

-- | An IO action for deciding whether or not to sample a trace.
--
-- Currently restricted to deny access to the B3 sampling state, but we may
-- want to be more flexible in the future.
type SamplingPolicy = IO SamplingDecision

sampleNever :: SamplingPolicy
sampleNever = pure SampleNever

sampleAlways :: SamplingPolicy
sampleAlways = pure SampleAlways

-- @sampleRandomly p@ returns `SampleAlways` with probability @p@ and
-- `SampleNever` with probability @1 - p@.
sampleRandomly :: Double -> SamplingPolicy
sampleRandomly samplingProbability
  | samplingProbability <= 0 = pure SampleNever
  | samplingProbability >= 1 = pure SampleAlways
  | otherwise = do
      x <- Random.uniformRM (0, 1) Random.globalStdGen
      pure $ if x < samplingProbability then SampleAlways else SampleNever

-- Like @sampleRandomly@, but with the probability expressed as the denominator
-- N of the fraction 1/N.
sampleOneInN :: Refined Positive Int -> SamplingPolicy
sampleOneInN denominator
  | n == 1 = pure SampleAlways
  | otherwise = do
      x <- Random.uniformRM (0, n - 1) Random.globalStdGen
      pure $ if x == 0 then SampleAlways else SampleNever
  where
    n = unrefine denominator
