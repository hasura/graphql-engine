import { useMemo, useRef } from 'react';
import { NodeRendererProps, Tree, TreeApi } from 'react-arborist';
import { QualifiedFunction, Table } from '../../../hasura-metadata-types';
import { DataSourceNode, DatabaseItemNode, isDataSourceNode } from '../types';
import { DataSourceTreeNode } from './DataSourceTreeNode';
import { DatabaseItemTreeNode } from './DatabaseItemTreeNode';
import { useTreeHeight } from './useTreeHeight';
function Node(
  props: NodeRendererProps<DataSourceNode> | NodeRendererProps<DatabaseItemNode>
) {
  if (isDataSourceNode(props)) return <DataSourceTreeNode {...props} />;

  return <DatabaseItemTreeNode {...props} />;
}

export type LeafType =
  | {
      dataSourceName: string;
      table: Table;
    }
  | {
      dataSourceName: string;
      function: QualifiedFunction;
    };

type TreeComponentProps = {
  treeData: DataSourceNode[];
  selection?: string;
  term?: string;
  onDatabaseClick?: (dataSourceName: string) => void;
  onLeafNodeClick?: (leaf: LeafType) => void;
};

export const TreeComponent = ({
  treeData,
  selection,
  term,
  onDatabaseClick,
  onLeafNodeClick,
}: TreeComponentProps) => {
  const ref = useRef<TreeApi<DataSourceNode>>();

  const data: DataSourceNode[] = useMemo(
    () =>
      treeData.map(source => {
        return {
          ...source,
          onClick: () => onDatabaseClick?.(source.dataSourceName),
          children: source.children?.map(leaf => ({
            ...leaf,
            onClick: () => {
              if ('table' in leaf)
                onLeafNodeClick?.({
                  dataSourceName: source.dataSourceName,
                  table: leaf.table,
                });
              else
                onLeafNodeClick?.({
                  dataSourceName: source.dataSourceName,
                  function: leaf.function,
                });
            },
          })),
        };
      }),
    [onDatabaseClick, onLeafNodeClick, treeData]
  );

  const { treeHeight } = useTreeHeight();

  return (
    <Tree
      data={data}
      openByDefault={false}
      width={'inherit'}
      indent={36}
      ref={ref}
      rowHeight={36}
      height={treeHeight}
      overscanCount={1}
      searchTerm={term}
      selection={selection}
      disableMultiSelection={true}
      searchMatch={(node, term) =>
        node.data.name.toLowerCase().includes(term.toLowerCase())
      }
    >
      {Node}
    </Tree>
  );
};
