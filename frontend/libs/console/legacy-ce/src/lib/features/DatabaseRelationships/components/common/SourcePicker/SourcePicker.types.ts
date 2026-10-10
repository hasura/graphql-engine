import { Table } from '@hasura/shared/types';

export type SourceSelectorItem =
  | {
      type: 'table';
      value: { dataSourceName: string; table: Table };
    }
  | {
      type: 'remoteSchema';
      value: { remoteSchemaName: string };
    };
