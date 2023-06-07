import { Table } from '../../../../features/hasura-metadata-types';
import { DataNode } from 'antd/lib/tree';
/*
 A GDC Source can be any user defined DB that can be added during run-time. We can only know a few properties during build time, such as name and kind
 which will be String, but for the tables - A GDC source can have any valid JSON definition for the `tables[i].table` property. The closest we can type is
 give it a Record<string, any> type to represent a valid JSON
*/
export type GDCSource = {
  name: string;
  kind: string;
  tables: {
    table: Table;
  }[];
};

export type GDCTreeData = DataNode[];
