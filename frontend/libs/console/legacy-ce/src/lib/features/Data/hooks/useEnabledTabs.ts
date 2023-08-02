import { Capabilities } from '@hasura/dc-api-types';
import { MetadataSelectors, useMetadata } from '../../hasura-metadata-api';
import { useDriverCapabilities } from './useDriverCapabilities';
import { Feature } from '../../DataSource';

function supportsRelationships(
  capabilities: Feature | Capabilities | undefined
) {
  if (!capabilities || capabilities === Feature.NotImplemented) {
    return false;
  }
  return Boolean(capabilities.relationships);
}

export type EnabledTabs = {
  browse: boolean;
  insert: boolean;
  modify: boolean;
  relationships: boolean;
  permissions: boolean;
};

export function getEnabledTabs(
  kind: string | undefined,
  capabilities: Feature | Capabilities | undefined
): EnabledTabs {
  return {
    browse: true,
    insert: true,
    modify: true,
    relationships: supportsRelationships(capabilities),
    permissions: true,
  };
}

export function useEnabledTabs(dataSourceName: string): EnabledTabs {
  const { data } = useMetadata(m =>
    MetadataSelectors.findSource(dataSourceName)(m)
  );
  const { data: capabilities } = useDriverCapabilities({ dataSourceName });
  return getEnabledTabs(data?.kind, capabilities);
}
