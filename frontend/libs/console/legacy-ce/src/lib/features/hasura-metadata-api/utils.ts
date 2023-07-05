import {
  Metadata,
  QualifiedFunction,
  Source,
  Table,
} from '../hasura-metadata-types';
import { areTablesEqual } from './areTablesEqual';

/*

  These utility functions are meant to be the lowest level of re-usable metadata utility operations.

  The flow of lowest level to higher level is:

    utility functions -> selectors -> useMetadata hook

  These functions are separated from the selectors so that a developer may make use of them within a custom selector.

  For example, if you are composing a custom selector to get table metadata comments,
  you can leverage the findMetadataTable() utility function alongside useMetadata() like this:

  const { isLoading, data: savedComment } = useMetadata(
    m => MetadataUtils.findMetadataTable(dataSourceName, table, m)?.configuration?.comment
  );

  These utility functions are also used within selectors.ts

  Add utility functions only if it would be helpful for common/universally needed operations.

*/

export const findMetadataSource = (dataSourceName: string, m: Metadata) =>
  m?.metadata.sources.find(s => s.name === dataSourceName);

export const findMetadataTable = (
  dataSourceName: string,
  table: Table,
  m: Metadata
) =>
  findMetadataSource(dataSourceName, m)?.tables.find(t =>
    areTablesEqual(t.table, table)
  );

export const findMetadataFunction = (
  dataSourceName: string,
  qualifiedFunction: QualifiedFunction,
  m: Metadata
) =>
  findMetadataSource(dataSourceName, m)?.functions?.find(fn =>
    areTablesEqual(fn.function, qualifiedFunction)
  );

export const getSupportsForeignKeys = (source: Source | undefined) =>
  source?.kind !== 'bigquery';
