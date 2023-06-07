import { FaCheck, FaExclamationTriangle, FaMinusCircle } from 'react-icons/fa';
import { Latency } from '../../../types';
import { Badge } from '../../../../../new-components/Badge';
import React from 'react';

export const LatencyBadge = ({
  latencies,
  dataSourceName,
}: {
  latencies: Latency[];
  dataSourceName: string;
}) => {
  const currentDataSourceLatencyInfo = latencies.find(
    latencyInfo => latencyInfo.dataSourceName === dataSourceName
  );

  if (!currentDataSourceLatencyInfo) return null;

  if (currentDataSourceLatencyInfo.avgLatency < 100)
    return (
      <Badge color="green">
        <FaCheck className="mr-xs" /> Connection
      </Badge>
    );

  if (currentDataSourceLatencyInfo.avgLatency < 200)
    return (
      <Badge color="yellow">
        <FaMinusCircle className="mr-xs" /> Acceptable
      </Badge>
    );

  return (
    <Badge color="red">
      <FaExclamationTriangle className="mr-xs" /> Elevated Latency
    </Badge>
  );
};
