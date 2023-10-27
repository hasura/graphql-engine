/**
 * Refer - https://github.com/hasura/graphql-engine-mono/blob/main/dc-agents/dc-api-types/src/models/TableInfo.ts
 */

import { ColumnValueGenerationStrategy } from '@hasura/dc-api-types';
import { GDCTable } from '..';

export type GetTableInfoResponse = {
  name: GDCTable;
  columns: {
    name: string;
    type: string;
    nullable: boolean;
    insertable: boolean;
    updatable: boolean;
    description: string;
    value_generated?: ColumnValueGenerationStrategy;
  }[];
  primary_key?: string[] | null;
  description?: string;
  foreign_keys?: Record<
    string,
    {
      foreign_table: GDCTable;
      column_mapping: Record<string, string>;
    }
  >;
  capabilities: {
    scalar_types: Record<string, any>;
  };
};
