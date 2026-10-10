import React, { useState } from 'react';
import clsx from 'clsx';
import { Checkbox, Flex } from '@radix-ui/themes';
import { FaChevronRight } from 'react-icons/fa';

export type TreeDataNode = {
  key: string;
  title?: React.ReactNode;
  icon?: React.ReactNode;
  children?: TreeDataNode[];
  /**
   * Hides this node's checkbox in a `checkable` tree.
   * @default true
   */
  checkable?: boolean;
  /**
   * Greys the node out and disables its checkbox and selection. It can still
   * be expanded.
   */
  disabled?: boolean;
  /**
   * Set to `false` to stop this node from being selected in a `selectable`
   * tree.
   * @default true
   */
  selectable?: boolean;
};

export type TreeProps<T extends TreeDataNode> = {
  treeData: T[];

  /**
   * Controls which nodes are expanded. Omit it to let the tree manage
   * expansion itself, starting from `defaultExpandedKeys`.
   */
  expandedKeys?: string[];
  /**
   * Nodes expanded on first render when `expandedKeys` is not controlled.
   * Their ancestors are expanded too, so the nodes are visible.
   */
  defaultExpandedKeys?: string[];
  onExpand?: (node: T, expanded: boolean) => void;

  /**
   * Renders a checkbox per node. Checking is strict: a node's state never
   * cascades to its parent or children.
   */
  checkable?: boolean;
  checkedKeys?: string[];
  onCheck?: (node: T, checked: boolean) => void;

  /**
   * Lets a node be selected by clicking its title.
   * @default false
   */
  selectable?: boolean;
  /**
   * Controls the selected node. Omit it to let the tree manage selection
   * itself, starting from `defaultSelectedKeys`.
   */
  selectedKeys?: string[];
  defaultSelectedKeys?: string[];
  onSelect?: (node: T) => void;

  /**
   * Called when a node's title is clicked, whether or not it is selectable.
   */
  onNodeClick?: (node: T) => void;

  /**
   * Renders each node's `icon` before its title.
   * @default false
   */
  showIcon?: boolean;
  /**
   * Icon for the expand toggle; it is rotated 90° when expanded.
   * @default <FaChevronRight />
   */
  switcherIcon?: React.ReactNode;
  /**
   * Stretches each title to the full row width.
   * @default false
   */
  blockNode?: boolean;

  className?: string;
  'aria-label'?: string;
};

const hasChildren = (node: TreeDataNode) => (node.children?.length ?? 0) > 0;

/**
 * Keys of the given nodes plus all their ancestors, so they end up visible.
 */
const withAncestorKeys = (treeData: TreeDataNode[], keys: string[]) => {
  const wanted = new Set(keys);
  const result = new Set(keys);

  const visit = (nodes: TreeDataNode[], ancestors: string[]): boolean =>
    nodes.reduce((found, node) => {
      const childFound = node.children
        ? visit(node.children, [...ancestors, node.key])
        : false;
      if (wanted.has(node.key) || childFound) {
        ancestors.forEach((key) => result.add(key));
        return true;
      }
      return found;
    }, false);

  visit(treeData, []);
  return Array.from(result);
};

export const Tree = <T extends TreeDataNode>({
  treeData,
  expandedKeys: controlledExpandedKeys,
  defaultExpandedKeys = [],
  onExpand,
  checkable = false,
  checkedKeys = [],
  onCheck,
  selectable = false,
  selectedKeys: controlledSelectedKeys,
  defaultSelectedKeys = [],
  onSelect,
  onNodeClick,
  showIcon = false,
  switcherIcon = <FaChevronRight />,
  blockNode = false,
  className,
  'aria-label': ariaLabel,
}: TreeProps<T>) => {
  const [internalExpandedKeys, setInternalExpandedKeys] = useState(() =>
    withAncestorKeys(treeData, defaultExpandedKeys),
  );
  const [internalSelectedKeys, setInternalSelectedKeys] =
    useState(defaultSelectedKeys);

  const expandedKeys = new Set(controlledExpandedKeys ?? internalExpandedKeys);
  const selectedKeys = new Set(controlledSelectedKeys ?? internalSelectedKeys);
  const checked = new Set(checkedKeys);

  const toggleExpanded = (node: T) => {
    const expanded = !expandedKeys.has(node.key);
    if (controlledExpandedKeys === undefined) {
      setInternalExpandedKeys((keys) =>
        expanded ? [...keys, node.key] : keys.filter((k) => k !== node.key),
      );
    }
    onExpand?.(node, expanded);
  };

  const isNodeCheckable = (node: T) => checkable && node.checkable !== false;

  // Mirrors rc-tree: a title click selects the node when it is selectable,
  // and otherwise toggles its checkbox.
  const handleTitleClick = (node: T) => {
    onNodeClick?.(node);
    if (node.disabled) return;
    if (selectable && node.selectable !== false) {
      if (controlledSelectedKeys === undefined) {
        setInternalSelectedKeys([node.key]);
      }
      onSelect?.(node);
    } else if (isNodeCheckable(node)) {
      onCheck?.(node, !checked.has(node.key));
    }
  };

  const renderNodes = (nodes: T[], level: number): React.ReactNode =>
    nodes.map((node) => {
      const expandable = hasChildren(node);
      const expanded = expandable && expandedKeys.has(node.key);
      const isSelectable =
        selectable && node.selectable !== false && !node.disabled;
      const isSelected = isSelectable && selectedKeys.has(node.key);

      return (
        <li
          key={node.key}
          role="treeitem"
          aria-level={level}
          aria-expanded={expandable ? expanded : undefined}
          aria-selected={isSelectable ? isSelected : undefined}
          aria-disabled={node.disabled || undefined}
        >
          <Flex
            className="gap-1 py-0.5"
            align="start"
            style={{ paddingLeft: `${(level - 1) * 1.5}rem` }}
          >
            {expandable ? (
              <button
                type="button"
                aria-label={expanded ? 'Collapse' : 'Expand'}
                onClick={() => toggleExpanded(node)}
                className="flex h-6 w-6 shrink-0 items-center justify-center rounded cursor-pointer focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-indigo-500"
              >
                <span
                  className={clsx(
                    'flex text-xs transition-transform',
                    expanded && 'rotate-90',
                  )}
                >
                  {switcherIcon}
                </span>
              </button>
            ) : (
              <span className="h-6 w-6 shrink-0" aria-hidden />
            )}

            {isNodeCheckable(node) ? (
              <span className="flex h-6 shrink-0 items-center">
                <Checkbox
                  checked={checked.has(node.key)}
                  disabled={node.disabled}
                  onCheckedChange={(value) => onCheck?.(node, value === true)}
                  aria-label={
                    typeof node.title === 'string' ? node.title : node.key
                  }
                />
              </span>
            ) : null}

            <div
              className={clsx(
                'flex min-h-6 items-center gap-1 rounded px-1',
                blockNode && 'flex-1',
                node.disabled ? 'cursor-not-allowed text-gray-400' : '',
                !node.disabled &&
                  (isSelectable || isNodeCheckable(node)) &&
                  'cursor-pointer',
                isSelected && 'text-indigo-900',
              )}
              onClick={() => handleTitleClick(node)}
              onKeyDown={
                isSelectable
                  ? (e) => {
                      if (e.key === 'Enter' || e.key === ' ') {
                        e.preventDefault();
                        handleTitleClick(node);
                      }
                    }
                  : undefined
              }
              tabIndex={isSelectable ? 0 : undefined}
            >
              {showIcon && node.icon ? (
                <span className="flex shrink-0 items-center">{node.icon}</span>
              ) : null}
              {node.title}
            </div>
          </Flex>

          {expanded ? (
            <ul role="group">{renderNodes(node.children as T[], level + 1)}</ul>
          ) : null}
        </li>
      );
    });

  return (
    <ul
      role="tree"
      aria-label={ariaLabel}
      className={clsx('text-sm', className)}
    >
      {renderNodes(treeData, 1)}
    </ul>
  );
};
