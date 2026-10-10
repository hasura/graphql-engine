import { useState } from 'react';
import { Meta, StoryObj } from '@storybook/react-webpack5';
import { FaFolder, FaTable } from 'react-icons/fa';
import { Tree, TreeDataNode } from './Tree';

export default {
  title: 'components/Tree',
  component: Tree,
} as Meta<typeof Tree>;

type Story = StoryObj<typeof Tree>;

const databaseTree: TreeDataNode[] = [
  {
    key: 'default',
    title: 'default',
    icon: <FaFolder />,
    selectable: false,
    children: [
      {
        key: 'default.public',
        title: 'public',
        icon: <FaFolder />,
        selectable: false,
        children: [
          { key: 'default.public.users', title: 'users', icon: <FaTable /> },
          { key: 'default.public.orders', title: 'orders', icon: <FaTable /> },
        ],
      },
    ],
  },
];

export const Selectable: Story = {
  args: {
    treeData: databaseTree,
    selectable: true,
    showIcon: true,
    defaultExpandedKeys: ['default.public.users'],
    defaultSelectedKeys: ['default.public.users'],
  },
};

const fieldsTree: TreeDataNode[] = [
  {
    key: 'query',
    title: 'Query',
    checkable: false,
    children: [
      { key: 'query.user', title: 'user' },
      { key: 'query.orders', title: 'orders' },
      {
        key: 'query.admin',
        title: 'admin (disabled)',
        disabled: true,
        children: [{ key: 'query.admin.id', title: 'id' }],
      },
    ],
  },
];

export const Checkable: Story = {
  render: () => {
    const [checkedKeys, setCheckedKeys] = useState<string[]>(['query.user']);
    const [expandedKeys, setExpandedKeys] = useState<string[]>(['query']);

    return (
      <Tree
        checkable
        blockNode
        treeData={fieldsTree}
        checkedKeys={checkedKeys}
        onCheck={(node, checked) =>
          setCheckedKeys((keys) =>
            checked ? [...keys, node.key] : keys.filter((k) => k !== node.key),
          )
        }
        expandedKeys={expandedKeys}
        onExpand={(node, expanded) =>
          setExpandedKeys((keys) =>
            expanded ? [...keys, node.key] : keys.filter((k) => k !== node.key),
          )
        }
      />
    );
  },
};
