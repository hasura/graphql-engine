import { TableEntry } from '@/metadata/types';
import { MetadataResponse } from './useMetadata';

export namespace MetadataSelector {
  export const getTablesFromAllSources = (
    m: MetadataResponse
  ): (TableEntry & { source: string })[] => {
    return (
      m.metadata?.sources.reduce((accTables, source) => {
        return accTables.concat(
          source.tables.map(t => ({ ...t, source: source.name }))
        );
      }, [] as (TableEntry & { source: string })[]) || []
    );
  };

  export const getSecuritySettings = (m: MetadataResponse) => {
    const { api_limits, graphql_schema_introspection } = m.metadata ?? {};
    return { api_limits, graphql_schema_introspection };
  };

  export const getDataSourceMetadata = (currentDataSource: string) => (
    m: MetadataResponse
  ) => {
    if (!currentDataSource) return;
    return m.metadata?.sources.find(
      source => source.name === currentDataSource
    );
  };

  export const getTables = (currentDataSource: string) => (
    m: MetadataResponse
  ) => {
    const sources = getDataSourceMetadata(currentDataSource)(m);
    return sources?.tables ?? [];
  };
}
