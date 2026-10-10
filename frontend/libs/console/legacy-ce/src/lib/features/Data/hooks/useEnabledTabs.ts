import { Capabilities } from '@hasura/dc-api-types';
import { useMetadata } from '@hasura/metadata/api';
import { useDriverCapabilities } from '@hasura/metadata/data-source';
import { MetadataSelectors } from '@hasura/metadata/helpers';

function supportsRelationships(capabilities: Capabilities | undefined) {
  return Boolean(capabilities?.relationships);
}

export type EnabledTabs = {
  browse: boolean;
  insert: boolean;
  modify: boolean;
  relationships: boolean;
  permissions: boolean;
};

export function getEnabledTabs(
  capabilities: Capabilities | undefined,
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
  const { data: source } = useMetadata(
    MetadataSelectors.findSource(dataSourceName),
  );

  const { data: capabilities } = useDriverCapabilities({ source });

  return getEnabledTabs(capabilities);
}
