import { DownOutlined } from '@ant-design/icons';
import { Tree } from 'antd';
import { Key } from 'antd/lib/table/interface';
import React from 'react';
import '../../../../features/RemoteRelationships/RemoteSchemaRelationships/components/RemoteSchemaTree/index.css';
import { GDCTreeData } from './types';

const checkForGDCRoute = () => window.location.pathname.includes('data/v2');

const getCurrentActiveKeys = () => {
  const activeKey: Key[] = [];

  const params = new URLSearchParams(window.location.search);

  const database = params.get('database');
  const table = params.get('table');

  if (table) {
    activeKey.push(JSON.stringify({ database, table: JSON.parse(table) }));
  } else {
    activeKey.push(JSON.stringify({ database }));
  }

  return activeKey;
};

type Props = {
  onSelect: (value: Key[]) => void;
  treeData: GDCTreeData;
};

export const GDCTree = ({ onSelect, treeData }: Props) => {
  const isGDCRouteActive = checkForGDCRoute();

  const activeKey = isGDCRouteActive ? getCurrentActiveKeys() : [];

  // unclear if we still need this check since this component won't be rendered
  // in the parent if "treeData" is empty
  if (!treeData || treeData.length === 0) return null;

  return (
    <Tree
      className="!text-gray-500"
      switcherIcon={<DownOutlined />}
      showIcon
      defaultExpandedKeys={activeKey} // there are only two variations here - DB level or table level. middle level selections cannot be accessed
      defaultSelectedKeys={activeKey}
      onSelect={onSelect}
      treeData={treeData}
    />
  );
};
