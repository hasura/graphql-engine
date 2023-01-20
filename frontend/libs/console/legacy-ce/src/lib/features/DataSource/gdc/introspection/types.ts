/**
 * Refer - https://github.com/hasura/graphql-engine-mono/blob/main/dc-agents/dc-api-types/src/models/TableInfo.ts
 */

import { GDCTable } from '..';

export type GetTableInfoResponse = {
  name: GDCTable;
  columns: { name: string; type: string; nullable: boolean }[];
  primary_key?: string[] | null;
  description?: string;
  foreign_keys?: Record<
    string,
    {
      foreign_table: GDCTable;
      column_mapping: Record<string, string>;
    }
  >;
};
