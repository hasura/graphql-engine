import { Table } from '../../../../hasura-metadata-types';

export type SourceSelectorItem =
  | {
      type: 'table';
      value: { dataSourceName: string; table: Table };
    }
  | {
      type: 'remoteSchema';
      value: { remoteSchemaName: string };
    };
