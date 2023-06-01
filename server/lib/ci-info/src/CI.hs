{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# HLINT ignore "avoid getEnvironment" #-}

module CI
  ( Types.CI (..),
    isCI,
    getCI,
  )
where

import CI.TH (getVendors)
import qualified CI.Types as Types
import Control.Arrow ((***))
import Data.Bool (bool)
import Data.Foldable (find)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (isJust)
import qualified Data.Text as T
import System.Environment (getEnvironment)

vendors :: [Types.Vendor]
vendors = $(getVendors)

getCI :: IO (Maybe Types.CI)
getCI = do
  env <- mkEnvMap <$> getEnvironment
  let maybeVendor = find (checkVendor env) vendors
  return $
    case maybeVendor of
      Nothing ->
        bool Nothing (Just Types.CI_UNKNOWN_VENDOR) $
          checkUnknownVendor env
      Just vendor -> Just $ Types.vendorConstant vendor
  where
    checkVendor env vendor = case Types.vendorEnv vendor of
      (Types.VendorEnvString text) -> HashMap.member text env
      (Types.VendorEnvList list) -> all (`HashMap.member` env) list
      (Types.VendorEnvObject hashMap) ->
        all
          (\(k, v) -> HashMap.lookup k env == Just v)
          $ HashMap.toList hashMap

    -- check vendor neutral environment variables
    checkUnknownVendor env = any (`HashMap.member` env) unknownVendorEnvVars

    unknownVendorEnvVars =
      map
        Types.EnvVarName
        [ "CI", -- Travis CI, CircleCI, Cirrus CI, Gitlab CI, Appveyor, CodeShip, dsari
          "CONTINUOUS_INTEGRATION", -- Travis CI, Cirrus CI
          "BUILD_NUMBER", -- Jenkins, TeamCity
          "RUN_ID" -- TaskCluster, dsari
        ]

    mkEnvMap =
      HashMap.fromList
        . map (Types.EnvVarName . T.pack *** Types.EnvVarValue . T.pack)

isCI :: IO Bool
isCI = isJust <$> getCI
