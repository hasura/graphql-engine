import React, { useState } from 'react';
import { NodeRendererProps, Tree } from 'react-arborist';
import { FaSearch } from 'react-icons/fa';
import { Input } from '../../../new-components/Form';
import { ReactQueryStatusUI } from '../../Data/components';
import { DATABASE_SEARCH_INPUT_ID } from '../constants';
import { getTreeIdFromUrl } from '../getTreeIdFromUrl';
import { DataSourceTreeNode, DatabaseItemTreeNode } from './components';
import { useTreeHeight } from './components/useTreeHeight';
import {
  DataSourceNode,
  DatabaseItemNode,
  LeafType,
  isDataSourceNode,
} from './types';
import { useNavTreeData } from './useNavTreeData';

type NavTreeProps = {
  handleDatabaseObjectClick?: (details: LeafType) => void;
  handleDatabaseClick?: (dataSourceName: string) => void;
};

const twInputStyles = `border-none shadow-none focus-visible:ring-0 p-3 text-lg h-auto`;

const getVisibleTreeNodes = () =>
  document
    .getElementById('tree-container')
    ?.querySelectorAll('[role="treeitem"]').length ?? 0;

const DataBoundNavTree = (props: NavTreeProps) => {
  const result = useNavTreeData();

  if (result.status !== 'success') {
    const { error, status, sourceCount } = result;
    return (
      <div className="m-3">
        <ReactQueryStatusUI
          status={status}
          loader="skeleton"
          skeletonProps={{ count: sourceCount ?? 5, height: 36 }}
          error={error}
        />
      </div>
    );
  }

  return <NavTreeUI {...props} treeData={result.treeData} />;
};

export const NavTreeUI = ({
  handleDatabaseClick,
  handleDatabaseObjectClick,
  treeData,
}: NavTreeProps & { treeData: DataSourceNode[] }) => {
  const [searchTerm, setSearchTerm] = useState('');

  const data: readonly DataSourceNode[] = React.useMemo(
    () =>
      treeData.map(source => {
        return {
          ...source,
          onClick: () => handleDatabaseClick?.(source.dataSourceName),
          children: source.children?.map(leaf => ({
            ...leaf,
            onClick: () => {
              if ('table' in leaf)
                handleDatabaseObjectClick?.({
                  dataSourceName: source.dataSourceName,
                  table: leaf.table,
                });
              else if ('function' in leaf)
                handleDatabaseObjectClick?.({
                  dataSourceName: source.dataSourceName,
                  function: leaf.function,
                });
            },
          })),
        };
      }),
    [handleDatabaseClick, handleDatabaseObjectClick, treeData]
  );

  const { treeHeight } = useTreeHeight();

  const [noResults, setNoResults] = useState(false);

  const onSearch = (query: string) => {
    setSearchTerm(query);

    // check at the end of the thread queue
    // the tree does it's own render, so we have to wait until the end of the thread queue to check for visible tree nodes
    // if we check right away, they are still on screen
    // and there's no re-render at this level when the tree updates via search so this is the only way to do it
    // the same issue arises even if you use the treeRef.current.visibleNodes
    setTimeout(() => {
      if (getVisibleTreeNodes() === 0 && !!query) {
        setNoResults(true);
      } else {
        setNoResults(false);
      }
    }, 0);
  };
  return (
    <div>
      <Input
        name={DATABASE_SEARCH_INPUT_ID}
        onChange={e => onSearch(e.target.value)}
        icon={<FaSearch />}
        inputClassName={twInputStyles}
        className="border-b"
      />
      <div id="tree-container" className="relative">
        {noResults && (
          <div className="absolute select-none text-muted text-lg top-0 mt-4 w-full text-center">
            No Results Found
          </div>
        )}
        <Tree
          data={data}
          openByDefault={false}
          width={'inherit'}
          indent={36}
          rowHeight={36}
          height={treeHeight}
          overscanCount={1}
          searchTerm={searchTerm}
          selection={getTreeIdFromUrl()}
          disableMultiSelection={true}
          searchMatch={(node, term) =>
            node.data.name.toLowerCase().includes(term.toLowerCase())
          }
        >
          {(
            // react-arborist render props:
            renderProps:
              | NodeRendererProps<DataSourceNode>
              | NodeRendererProps<DatabaseItemNode>
          ) =>
            isDataSourceNode(renderProps) ? (
              <DataSourceTreeNode {...renderProps} />
            ) : (
              <DatabaseItemTreeNode {...renderProps} />
            )
          }
        </Tree>
      </div>
    </div>
  );
};

export const NavTree = DataBoundNavTree;
