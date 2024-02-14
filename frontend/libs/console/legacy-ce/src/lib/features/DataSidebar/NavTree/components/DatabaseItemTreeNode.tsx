import clsx from 'clsx';
import { NodeRendererProps } from 'react-arborist';
import { CgSpinner } from 'react-icons/cg';
import { FaTable } from 'react-icons/fa';
import { TbMathFunction } from 'react-icons/tb';
import {
  useIsAnythingLoading,
  useIsDatabaseItemLoading,
} from '../../SidebarContext';
import { styles } from '../../styles';
import { DatabaseItemNode } from '../types';
import { HighlightText } from './HighlightText';

export const DatabaseItemTreeNode = ({
  node,
  tree,
  style,
}: NodeRendererProps<DatabaseItemNode>) => {
  const loading = useIsDatabaseItemLoading({
    dataSourceName: node.data.dataSourceName,
    item: node.data.type === 'table' ? node.data.table : node.data.function,
    type: node.data.type,
  });

  const treeLoading = useIsAnythingLoading();

  return (
    <button
      disabled={loading || treeLoading}
      onClick={() => {
        if (loading || treeLoading) return;

        node.data?.onClick?.();
      }}
      className={clsx(
        styles.sideBarItem.default,
        styles.treeItem,
        node.isSelected && styles.sideBarItem.active
      )}
      style={style}
    >
      {node.data.type === 'table' ? (
        <FaTable className="text-gray-600" />
      ) : (
        <TbMathFunction className="text-gray-600" />
      )}
      <HighlightText
        className="text-lg whitespace-nowrap overflow-hidden text-ellipsis"
        text={node.data.name}
        highlightedText={tree.searchTerm}
      />
      {loading && <CgSpinner className="animate-spin" size={20} />}
    </button>
  );
};
