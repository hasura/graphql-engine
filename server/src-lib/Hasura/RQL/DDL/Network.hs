module Hasura.RQL.DDL.Network
  ( checkForHostnameWithSuffixInAllowlistObject,
    dropHostFromAllowList,
    runAddHostToTLSAllowlist,
    runDropHostFromTLSAllowlist,
  )
where

import Data.Text (pack)
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Metadata.Class ()
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.SchemaCache.Build
import Network.Types.Extended

runAddHostToTLSAllowlist ::
  (QErrM m, CacheRWM m, MetadataM m) =>
  TlsAllow ->
  m EncJSON
runAddHostToTLSAllowlist tlsAllowListEntry@TlsAllow {..} = do
  networkMetadata <- _metaNetwork <$> getMetadata

  when (null taHost) $ do
    throw400 BadRequest $ "key \"host\" cannot be empty"

  when (checkForHostWithSuffixInTLSAllowlist taHost taSuffix (tlsList networkMetadata)) $ do
    case taSuffix of
      Nothing ->
        throw400 AlreadyExists
          $ "the host "
          <> dquote (pack taHost)
          <> " already exists in the allowlist"
      Just suffix ->
        throw400 AlreadyExists
          $ "the host "
          <> dquote (pack taHost)
          <> " with suffix "
          <> dquote (pack suffix)
          <> " already exists in the allowlist"

  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ addHostToTLSAllowList tlsAllowListEntry

  pure successMsg
  where
    tlsList nm = networkTlsAllowlist nm

runDropHostFromTLSAllowlist ::
  (QErrM m, CacheRWM m, MetadataM m) =>
  DropHostFromTLSAllowlist ->
  m EncJSON
runDropHostFromTLSAllowlist (DropHostFromTLSAllowlist hostname maybeSuffix) = do
  networkMetadata <- _metaNetwork <$> getMetadata

  when (null hostname) $ do
    throw400 BadRequest $ "hostname cannot be empty"

  unless (checkForHostWithSuffixInTLSAllowlist hostname maybeSuffix (networkTlsAllowlist networkMetadata)) $ do
    case maybeSuffix of
      Nothing ->
        throw400 NotExists
          $ "the host "
          <> dquote (pack hostname)
          <> " isn't present in the allowlist"
      Just suffix ->
        throw400 NotExists
          $ "the host "
          <> dquote (pack hostname)
          <> " with suffix "
          <> dquote (pack suffix)
          <> " isn't present in the allowlist"

  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ dropHostFromAllowList hostname maybeSuffix

  pure successMsg

addHostToTLSAllowList :: TlsAllow -> MetadataModifier
addHostToTLSAllowList tlsaObj = MetadataModifier $ \m ->
  m {_metaNetwork = Network $ (tlsList m) ++ [tlsaObj]}
  where
    tlsList md = networkTlsAllowlist (_metaNetwork md)

dropHostFromAllowList :: String -> Maybe String -> MetadataModifier
dropHostFromAllowList host maybeSuffix = MetadataModifier $ \m ->
  m {_metaNetwork = Network $ filteredList m}
  where
    tlsList md = networkTlsAllowlist (_metaNetwork md)

    filteredList md = filter (not . checkForHostnameWithSuffixInAllowlistObject host maybeSuffix) (tlsList md)

checkForHostnameWithSuffixInAllowlistObject :: String -> Maybe String -> TlsAllow -> Bool
checkForHostnameWithSuffixInAllowlistObject host maybeSuffix tlsa = host == (taHost tlsa) && maybeSuffix == (taSuffix tlsa)

checkForHostWithSuffixInTLSAllowlist :: String -> Maybe String -> [TlsAllow] -> Bool
checkForHostWithSuffixInTLSAllowlist host maybeSuffix tlsAllowList =
  any (checkForHostnameWithSuffixInAllowlistObject host maybeSuffix) tlsAllowList
