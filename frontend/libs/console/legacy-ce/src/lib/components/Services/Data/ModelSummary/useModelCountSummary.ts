import { useMetadata } from '../../../../features/hasura-metadata-api';
import { Metadata } from '../../../../features/hasura-metadata-types';
import { SourceLevelSummary } from './ModelSummary';

const MONGO_DB_TYPE = 'mongodb';

export const selectModelSummaryData = (m: Metadata) => {
  const tablesAndViews: SourceLevelSummary[] = m.metadata.sources
    .filter(source => source.kind !== MONGO_DB_TYPE)
    .map(source => ({
      dataSourceName: source.name,
      totalCount: source.tables.length,
    }));

  const collections: SourceLevelSummary[] = m.metadata.sources
    .filter(source => source.kind === MONGO_DB_TYPE)
    .map(source => ({
      dataSourceName: source.name,
      totalCount: source.tables.length,
    }));

  const logicalModels: SourceLevelSummary[] = m.metadata.sources
    .filter(source => source.kind !== MONGO_DB_TYPE)
    .map(source => ({
      dataSourceName: source.name,
      totalCount: source?.logical_models?.length ?? 0,
    }));

  return { tablesAndViews, collections, logicalModels };
};

export const useModelCountSummary = () => {
  return useMetadata(m => selectModelSummaryData(m));
};
