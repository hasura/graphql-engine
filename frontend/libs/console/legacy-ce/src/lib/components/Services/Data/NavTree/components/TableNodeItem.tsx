import { TableNode } from '../types';
import clsx from 'clsx';
import { NodeRendererProps } from 'react-arborist';
import { FaTable } from 'react-icons/fa';
import { GetHighlightedText } from './GetHighlightedText';

export const TableNodeItem = ({
  node,
  tree,
  style,
}: NodeRendererProps<TableNode>) => {
  return (
    <div className="flex gap-0.5 items-center" style={style}>
      <div
        className={clsx(
          'flex items-center px-1.5 py-1.5 hover:rounded hover:bg-gray-200 hover:cursor-pointer',
          node.isSelected ? 'bg-gray-200 rounded' : ''
        )}
        onClick={() => {
          node.data?.onClick?.();
        }}
      >
        <FaTable className="text-gray-600" />

        <span className="ml-1.5">
          <GetHighlightedText
            text={node.data.name}
            highlight={tree.searchTerm}
          />
        </span>
      </div>
    </div>
  );
};
