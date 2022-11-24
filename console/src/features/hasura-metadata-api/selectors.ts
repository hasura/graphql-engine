import { Metadata, Table } from '@/features/hasura-metadata-types';
import { areTablesEqual } from './utils';

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

const getMetadataSources = () => (m?: Metadata) => {
  return m?.metadata.sources;
};

const findMetadataSource = (dataSourceName?: string) => (m?: Metadata) => {
  return m?.metadata.sources.find(s => s.name === dataSourceName);
};

const getMetadataTables = (dataSourceName: string) => (m?: Metadata) => {
  return findMetadataSource(dataSourceName)(m)?.tables;
};

const findMetadataTable =
  (dataSourceName: string, table: Table) => (m?: Metadata) => {
    return findMetadataSource(dataSourceName)(m)?.tables.find(t =>
      areTablesEqual(t.table, table)
    );
  };

const getMetadataResourceVersion = () => (m?: Metadata) => {
  return m?.resource_version;
};

export const MetadataSelector = {
  getMetadataSources,
  findMetadataSource,
  getMetadataTables,
  findMetadataTable,
  getMetadataResourceVersion,
};
