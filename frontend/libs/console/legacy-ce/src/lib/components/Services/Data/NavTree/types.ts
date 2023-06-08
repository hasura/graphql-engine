import { NodeRendererProps } from 'react-arborist';
import {
  QualifiedFunction,
  Table,
} from '../../../../features/hasura-metadata-types';
import { InconsistentObject } from '../../../../features/hasura-metadata-api';
import { ReleaseType } from '../../../../features/DataSource';

export type TableNode = {
  id: string;
  table: Table;
  onClick?: () => void;
  // needed for search
  name: string;
};

export type FunctionNode = {
  id: string;
  function: QualifiedFunction;
  onClick?: () => void;
  // needed for search
  name: string;
};

export type DataSourceNode = {
  id: string;
  dataSourceName: string;
  driver: string;
  releaseType?: ReleaseType;
  onClick?: () => void;
  children?: (TableNode | FunctionNode)[];
  inconsistentObject?: InconsistentObject;
  // needed for search
  name: string;
};

export const isDataSourceNode = (
  value:
    | NodeRendererProps<TableNode>
    | NodeRendererProps<DataSourceNode>
    | NodeRendererProps<FunctionNode>
): value is NodeRendererProps<DataSourceNode> => {
  return 'dataSourceName' in value.node.data;
};

export const isFunctionNode = (
  value:
    | NodeRendererProps<TableNode>
    | NodeRendererProps<DataSourceNode>
    | NodeRendererProps<FunctionNode>
): value is NodeRendererProps<FunctionNode> => {
  return 'function' in value.node.data;
};

export const isTableNode = (
  value:
    | NodeRendererProps<TableNode>
    | NodeRendererProps<DataSourceNode>
    | NodeRendererProps<FunctionNode>
): value is NodeRendererProps<TableNode> => {
  return 'table' in value.node.data;
};
