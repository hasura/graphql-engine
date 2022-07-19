import { DownOutlined } from '@ant-design/icons';
import { Tree } from 'antd';
import { Key } from 'antd/lib/table/interface';
import React from 'react';
import { useTreeData } from './hooks/useTreeData';
import '../../../../features/RemoteRelationships/RemoteSchemaRelationships/components/RemoteSchemaTree/index.css';
import { GDCSource } from './types';

const checkForGDCRoute = () => window.location.pathname.includes('data/v2');

const getCurrentActiveKeys = () => {
  const activeKey: Key[] = [];

  const params = new URLSearchParams(window.location.search);

  const database = params.get('database');
  const table = params.get('table');

  if (table) {
    activeKey.push(JSON.stringify({ database, ...JSON.parse(table) }));
  } else {
    activeKey.push(JSON.stringify({ database }));
  }

  return activeKey;
};

type Props = {
  onSelect: (value: Key[]) => void;
};

/* 
  This component is still very much in development and will be changed once we have an API that tells us about the hierarchy of a GDC source
  Until then, this component is more or less a POC/experminatal in nature and tests for its accompaniying story have not been included for this reason.
  If you wish to test out this component, head over to src/utils/featureFlags.ts and edit the GDC_TREE_VIEW_DEV to enabled to view it the console with mock data
*/

export const GDCTree = <TreeGDCSource extends GDCSource = GDCSource>(
  props: Props
) => {
  const isGDCRouteActive = checkForGDCRoute();

  const activeKey = isGDCRouteActive ? getCurrentActiveKeys() : [];

  const { data: gdcDatabases } = useTreeData<TreeGDCSource>();

  if (!gdcDatabases || gdcDatabases.length === 0) return null;

  return (
    <Tree
      className="!text-gray-500"
      switcherIcon={<DownOutlined />}
      showIcon
      defaultExpandedKeys={activeKey} // there are only two variations here - DB level or table level. middle level selections cannot be accessed
      defaultSelectedKeys={activeKey}
      onSelect={props.onSelect}
      treeData={gdcDatabases}
    />
  );
};
