import { useMemo } from 'react';
import { FaCheck, FaExclamationTriangle, FaMinusCircle } from 'react-icons/fa';
import ToolTip from '../../../../../../lib/components/Common/Tooltip/Tooltip';
import { Badge, BadgeColor } from '../../../../../new-components/Badge';
import { Latency } from '../../../types';

type AvgLatency = Latency['avgLatency'];

type GetBadgeProps = {
  color: BadgeColor;
  icon: React.ReactNode;
  label: string;
};

type LatencyBadgeProps = {
  latencies: Latency[];
  dataSourceName: string;
};

const getMessage = (avgLatency: AvgLatency) =>
  `Latency: ${Math.ceil(avgLatency)} ms`;

const getBadgeProps = (avgLatency: AvgLatency): GetBadgeProps => {
  if (avgLatency < 100) {
    return {
      color: 'green',
      icon: <FaCheck className="mr-xs" />,
      label: 'Connection',
    };
  } else if (avgLatency < 200) {
    return {
      color: 'yellow',
      icon: <FaMinusCircle className="mr-xs" />,
      label: 'Acceptable',
    };
  } else {
    return {
      color: 'red',
      icon: <FaExclamationTriangle className="mr-xs" />,
      label: 'Elevated Latency',
    };
  }
};

export const LatencyBadge = ({
  latencies = [],
  dataSourceName,
}: LatencyBadgeProps) => {
  const currentDataSourceLatencyInfo = useMemo(
    () =>
      latencies.find(
        latencyInfo => latencyInfo.dataSourceName === dataSourceName
      ),
    [latencies, dataSourceName]
  );

  if (!currentDataSourceLatencyInfo) {
    return null;
  }

  const { avgLatency } = currentDataSourceLatencyInfo || {};
  const message = getMessage(avgLatency);
  const badgeProps = getBadgeProps(avgLatency);

  return (
    <ToolTip message={message} placement="top">
      <Badge color={badgeProps.color}>
        {badgeProps.icon} {badgeProps.label}
      </Badge>
    </ToolTip>
  );
};
