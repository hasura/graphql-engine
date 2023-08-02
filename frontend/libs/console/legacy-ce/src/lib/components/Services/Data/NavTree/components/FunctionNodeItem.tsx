import { TbMathFunction } from 'react-icons/tb';
import { FunctionNode } from '../types';
import clsx from 'clsx';
import { NodeRendererProps } from 'react-arborist';
import { GetHighlightedText } from './GetHighlightedText';

export const FunctionNodeItem = ({
  node,
  tree,
  style,
}: NodeRendererProps<FunctionNode>) => {
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
        <TbMathFunction className="text-gray-600" />

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
