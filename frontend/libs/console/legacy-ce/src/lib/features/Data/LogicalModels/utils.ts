import { Metadata } from '../../hasura-metadata-types';
import { LogicalModelWithSource, NativeQueryWithSource } from './types';

export const extractModelsAndQueriesFromMetadata = (
  m: Metadata
): { queries: NativeQueryWithSource[]; models: LogicalModelWithSource[] } => {
  const sources = m.metadata.sources;
  let models: LogicalModelWithSource[] = [];
  let queries: NativeQueryWithSource[] = [];

  sources.forEach(s => {
    if (s.logical_models && s.logical_models.length > 0) {
      models = [
        ...models,
        ...s.logical_models.map(m => ({ ...m, source: s.name })),
      ];
    }

    if (s.native_queries && s.native_queries.length > 0) {
      queries = [
        ...queries,
        ...s.native_queries.map(q => ({ ...q, source: s.name })),
      ];
    }
  });

  return {
    models,
    queries,
  };
};
