import { NodeRendererProps } from 'react-arborist';
import { DataSourceNode } from '../types';
import { FaAngleDown, FaAngleRight, FaDatabase } from 'react-icons/fa';
import clsx from 'clsx';
import { Tooltip } from '../../../../../new-components/Tooltip';
import { Badge } from '../../../../../new-components/Badge';
import { GetHighlightedText } from './GetHighlightedText';

const StatusReport = ({
  node,
  tree,
  style,
}: NodeRendererProps<DataSourceNode>) => {
  const details = {
    Driver: <div>{node.data.driver}</div>,
    Release: (
      <div>
        <Badge>
          {node.data.releaseType === 'GA' ? 'Stable' : node.data.releaseType}
        </Badge>
      </div>
    ),
    Status: node.data.inconsistentObject ? (
      <span className="text-red-400">Inconsistent</span>
    ) : (
      <span className="text-green-400">Active</span>
    ),
    Errors: node.data.inconsistentObject ? (
      <div className="bg-white break-words max-h-[320px] max-w-sm overflow-y-scroll p-2 rounded text-black whitespace-pre-line">
        {JSON.stringify(node.data.inconsistentObject)}
      </div>
    ) : null,
  };

  return (
    <table className="border-separate">
      <tbody>
        {Object.entries(details)
          .filter(([key, value]) => !!value)
          .map(([key, value]) => {
            return (
              <tr className="row">
                <td className="col flex">
                  <div className="font-bold">{key}</div>
                </td>
                <td className="col">{value}</td>
              </tr>
            );
          })}
      </tbody>
    </table>
  );
};

export const DatabaseNodeItem = (props: NodeRendererProps<DataSourceNode>) => {
  const { node, tree, style } = props;
  return (
    <div className="flex gap-0.5 items-center" style={style}>
      {node.children?.length ? (
        node.isOpen ? (
          <FaAngleDown
            data-testid={`${node.data.name}-close`}
            className="text-gray-600 hover:cursor-pointer"
            onClick={() => {
              node.toggle();
            }}
          />
        ) : (
          <FaAngleRight
            data-testid={`${node.data.name}-expand`}
            className="text-gray-600 hover:cursor-pointer"
            onClick={() => node.toggle()}
          />
        )
      ) : (
        <FaAngleRight className="text-gray-400" />
      )}

      <div
        className={clsx(
          'flex items-center py-1.5 hover:rounded hover:bg-gray-200 hover:cursor-pointer',
          node.isSelected ? 'bg-gray-200 rounded' : ''
        )}
        onClick={() => {
          node.data?.onClick?.();
        }}
      >
        <span className="relative">
          {node.data.inconsistentObject && (
            <span
              id="dot"
              className="rounded-lg absolute h-[10px] w-[10px] top-0 right-[-5px] bg-red-600"
              data-testid={`${node.data.name}-error-indicator`}
            />
          )}

          <Tooltip
            tooltipContentChildren={<StatusReport {...props} />}
            side="bottom"
            align="start"
          >
            <FaDatabase className="text-gray-600 text-gray-600 text-lg" />
          </Tooltip>
        </span>

        <span className="ml-sm">
          <GetHighlightedText
            text={node.data.name}
            highlight={tree.searchTerm}
          />
        </span>

        {node.children?.length ? (
          <span
            className="bg-gray-300 ml-1.5 mr-1.5 px-1.5 py-0.5 rounded text-xs"
            data-testId={`${node.data.name}-object-count`}
          >
            {node.children?.length}
          </span>
        ) : null}
      </div>
    </div>
  );
};
