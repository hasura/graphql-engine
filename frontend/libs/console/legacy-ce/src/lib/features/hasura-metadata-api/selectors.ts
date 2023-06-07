import {
  LogicalModelWithSource,
  NativeQueryWithSource,
} from '../Data/LogicalModels/types';
import { Metadata, Table } from '../hasura-metadata-types';
import * as utils from './utils';

/* 

How do I implement my custom selector apart from the ones provided here?

In your local component, write the selector as per your requirement ->

// use the useMetadata from lib
import {useMetadata} from '@/features/hasura-metadata-api`;

// This will be your selector
const getRemoteSchema = (remoteSchemaName: string) => (m: Metadata) => m.metadata.remote_schemas?.find(s => s.name === remoteSchemaName)

// usage
const { isLoading, data: remote_schemas } = useMetadata(getRemoteSchema('foobar'));

*/

export const getSources = () => (m: Metadata) => m?.metadata.sources;

export const findSource = (dataSourceName: string) => (m: Metadata) =>
  utils.findMetadataSource(dataSourceName, m);

export const getTables = (dataSourceName: string) => (m: Metadata) =>
  utils.findMetadataSource(dataSourceName, m)?.tables;

export const findTable =
  (dataSourceName: string, table: Table) => (m: Metadata) =>
    utils.findMetadataTable(dataSourceName, table, m);

export const resourceVersion = () => (m: Metadata) => m?.resource_version;

export const extractModelsAndQueriesFromMetadata = (
  m: Metadata
): { queries: NativeQueryWithSource[]; models: LogicalModelWithSource[] } => {
  const sources = m.metadata.sources;
  let models: LogicalModelWithSource[] = [];
  let queries: NativeQueryWithSource[] = [];

  sources.forEach(s => {
    if (s.logical_models && s.logical_models.length > 0) {
      models = [...models, ...s.logical_models.map(m => ({ ...m, source: s }))];
    }

    if (s.native_queries && s.native_queries.length > 0) {
      queries = [
        ...queries,
        ...s.native_queries.map(q => ({ ...q, source: s })),
      ];
    }
  });

  return {
    models,
    queries,
  };
};
