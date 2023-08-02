import { NodeRendererProps, Tree } from 'react-arborist';
import {
  DataSourceNode,
  FunctionNode,
  TableNode,
  isDataSourceNode,
  isFunctionNode,
  // isTableNode,
} from '../types';
import { DatabaseNodeItem } from './DatabaseNodeItem';
import { FunctionNodeItem } from './FunctionNodeItem';
import { TableNodeItem } from './TableNodeItem';
import {
  QualifiedFunction,
  Table,
} from '../../../../../features/hasura-metadata-types';

function Node(
  props:
    | NodeRendererProps<DataSourceNode>
    | NodeRendererProps<TableNode>
    | NodeRendererProps<FunctionNode>
) {
  // const nodeData = node.data;

  if (isDataSourceNode(props)) return <DatabaseNodeItem {...props} />;

  if (isFunctionNode(props)) return <FunctionNodeItem {...props} />;

  return <TableNodeItem {...props} />;
}

export const TreeComponent = (props: {
  treeData: DataSourceNode[];
  selection?: string;
  term?: string;
  onDatabaseClick?: (dataSourceName: string) => void;
  onLeafNodeClick?: (
    leaf:
      | {
          dataSourceName: string;
          table: Table;
        }
      | {
          dataSourceName: string;
          function: QualifiedFunction;
        }
  ) => void;
}) => {
  const data: DataSourceNode[] = props.treeData.map(source => {
    return {
      ...source,
      onClick: () => props.onDatabaseClick?.(source.dataSourceName),
      children: source.children?.map(leaf => ({
        ...leaf,
        onClick: () => {
          if ('table' in leaf)
            props.onLeafNodeClick?.({
              dataSourceName: source.dataSourceName,
              table: leaf.table,
            });
          else
            props.onLeafNodeClick?.({
              dataSourceName: source.dataSourceName,
              function: leaf.function,
            });
        },
      })),
    };
  });

  return (
    <Tree
      initialData={data}
      openByDefault={false}
      width={'inherit'}
      indent={24}
      rowHeight={36}
      height={800}
      overscanCount={1}
      searchTerm={props.term}
      padding={15}
      selection={props.selection}
      disableMultiSelection={true}
      searchMatch={(node, term) =>
        node.data.name.toLowerCase().includes(term.toLowerCase())
      }
    >
      {Node}
    </Tree>
  );
};
