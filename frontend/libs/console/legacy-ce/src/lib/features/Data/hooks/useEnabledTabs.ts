import { MetadataSelectors, useMetadata } from '../../hasura-metadata-api';

export type EnabledTabs = {
  browse: boolean;
  insert: boolean;
  modify: boolean;
  relationships: boolean;
  permissions: boolean;
};

export function getEnabledTabs(kind: string | undefined): EnabledTabs {
  return {
    browse: true,
    insert: true,
    modify: true,
    relationships: true,
    permissions: true,
  };
}

export function useEnabledTabs(dataSourceName: string): EnabledTabs {
  const { data } = useMetadata(m =>
    MetadataSelectors.findSource(dataSourceName)(m)
  );
  return getEnabledTabs(data?.kind);
}
