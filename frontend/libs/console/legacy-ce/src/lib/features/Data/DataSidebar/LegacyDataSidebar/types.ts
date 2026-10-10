import { Table } from '@hasura/shared/types';

export type SourceItemsTypes =
  'database' | 'schema' | 'view' | 'enum' | 'function' | 'table';

export type SourceItem = {
  name: string;
  schema?: string;
  tableName: string;
  table: Table;
  source: string;
  type: SourceItemsTypes;
  children?: SourceItem[];
};

export const activeStyle = {
  color: '#fd9540',
};

export const legacyIconStyles = 'ml-2 mr-[15px]';
