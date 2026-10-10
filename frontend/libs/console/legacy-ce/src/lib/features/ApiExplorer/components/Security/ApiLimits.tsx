import React from 'react';
import { SecurityTabs } from './SecurityTabs';
import LimitsTable from './LimitsTable';

const headers = [
  'Role',
  'Depth Limit',
  'Node Limit',
  'Rate Limit (RPM)',
  'Timeout (Seconds)',
  'Batch Request Limit',
];
const keys = [
  'role',
  'depth_limit',
  'node_limit',
  'rate_limit',
  'time_limit',
  'batch_limit',
];

const ApiLimitsComponent: React.FC = () => {
  return (
    <SecurityTabs tabName="api_limits">
      <LimitsTable headers={headers} keys={keys} />
    </SecurityTabs>
  );
};

export default ApiLimitsComponent;
