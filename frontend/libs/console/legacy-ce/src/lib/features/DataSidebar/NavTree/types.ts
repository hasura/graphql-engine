import { NodeRendererProps } from 'react-arborist';
import { QualifiedFunction, Table } from '../../hasura-metadata-types';
import { InconsistentObject } from '../../hasura-metadata-api';
import { ReleaseType } from '../../DataSource';

export type DatabaseItemNode = {
  id: string;
  onClick?: () => void;
  // needed for search
  name: string;
  dataSourceName: string;
} & (
  | {
      type: 'table';
      table: Table;
    }
  | {
      type: 'function';
      function: QualifiedFunction;
    }
);

export type DataSourceNode = {
  id: string;
  dataSourceName: string;
  driver: string;
  releaseType?: ReleaseType;
  onClick?: () => void;
  children?: DatabaseItemNode[];
  inconsistentObject?: InconsistentObject;
  // needed for search
  name: string;
};

export const isDataSourceNode = (
  value: NodeRendererProps<DataSourceNode> | NodeRendererProps<DatabaseItemNode>
): value is NodeRendererProps<DataSourceNode> => {
  return 'driver' in value.node.data;
};

export type LeafType =
  | {
      dataSourceName: string;
      table: Table;
    }
  | {
      dataSourceName: string;
      function: QualifiedFunction;
    };
