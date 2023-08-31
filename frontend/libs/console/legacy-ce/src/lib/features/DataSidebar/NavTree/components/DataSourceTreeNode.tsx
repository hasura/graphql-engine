import clsx from 'clsx';
import { useEffect } from 'react';
import { NodeRendererProps } from 'react-arborist';
import { BsFillExclamationTriangleFill } from 'react-icons/bs';
import { CgSpinner } from 'react-icons/cg';
import { RxTriangleRight } from 'react-icons/rx';
import { Badge } from '../../../../new-components/Badge';
import { Tooltip } from '../../../../new-components/Tooltip';
import {
  useIsAnythingLoading,
  useIsDataSourceLoading,
} from '../../SidebarContext';
import { DatabaseIcon } from '../../constants';
import { styles } from '../../styles';
import { DataSourceNode } from '../types';
import { HighlightText } from './HighlightText';

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

export const DataSourceTreeNode = (
  props: NodeRendererProps<DataSourceNode>
) => {
  const { node, tree, style } = props;
  const isInconsistent = !!node.data?.inconsistentObject;
  const DbIcon = isInconsistent ? BsFillExclamationTriangleFill : DatabaseIcon;
  const isLoading = useIsDataSourceLoading(node.data.dataSourceName);
  const treeLoading = useIsAnythingLoading();

  const hasChildren = !!node.children?.length;

  useEffect(() => {
    if (node.isSelected && node.isClosed) {
      node.open();
    }
  }, []);

  return (
    <button
      disabled={isLoading || treeLoading}
      onClick={e => {
        if (isLoading || treeLoading) return;

        // only send click event if user is opening the node
        if (node.isClosed && !isLoading && !treeLoading) {
          node.data?.onClick?.();
          node.open();
          node.select();
        } else {
          node.close();

          // we close the data source node without moving selection to it
          e.preventDefault();
          e.stopPropagation();
        }
      }}
      className={clsx(
        styles.sideBarItem.default,
        node.isSelected && styles.sideBarItem.active,
        styles.treeItem
      )}
      style={style}
    >
      <div className="ml-2">
        <RxTriangleRight
          className={clsx(
            'transition ease-in-out text-gray-600',
            node.isOpen ? 'rotate-90' : 'rotate-0',
            !hasChildren && 'opacity-0'
          )}
        />
        <Tooltip
          className="ml-0"
          tooltipContentChildren={<StatusReport {...props} />}
          side="bottom"
          align="start"
        >
          <DbIcon
            className={clsx(
              'text-gray-600',
              isInconsistent && 'text-red-600 animate-pulse'
            )}
            size={25}
          />
        </Tooltip>
      </div>
      <HighlightText
        className="text-[1.25rem]"
        text={node.data.name}
        highlightedText={tree.searchTerm}
      />
      {hasChildren ? (
        <Badge color={'green'} className="rounded-md px-2 font-normal">
          {node.children?.length}
        </Badge>
      ) : (
        <div className="muted italic text-gray-500 text-sm">
          (No Tracked Items)
        </div>
      )}
      {isLoading && <CgSpinner className="animate-spin" size={20} />}
    </button>
  );
};
