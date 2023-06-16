import { useMemo } from 'react';
import {
  FaCheck,
  FaExclamationCircle,
  FaExclamationTriangle,
  FaMinusCircle,
} from 'react-icons/fa';
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
  if (avgLatency >= 200) {
    return {
      color: 'red',
      icon: <FaExclamationTriangle className="mr-xs" />,
      label: 'Elevated Latency',
    };
  }

  if (avgLatency >= 100 && avgLatency < 200) {
    return {
      color: 'yellow',
      icon: <FaMinusCircle className="mr-xs" />,
      label: 'Acceptable',
    };
  }

  if (avgLatency > 0) {
    return {
      color: 'green',
      icon: <FaCheck className="mr-xs" />,
      label: 'Connection',
    };
  }

  return {
    color: 'light-gray',
    icon: <FaExclamationCircle className="mr-xs" />,
    label: 'Failed to get latency',
  };
};

export const LatencyBadge = ({
  latencies = [],
  dataSourceName,
}: LatencyBadgeProps) => {
  const { avgLatency, hasError } = useMemo(
    () => ({
      avgLatency:
        latencies.find(
          latencyInfo => latencyInfo.dataSourceName === dataSourceName
        )?.avgLatency || 0,
      hasError: latencies.find(
        latencyInfo =>
          latencyInfo.dataSourceName === dataSourceName && !!latencyInfo.error
      ),
    }),
    [latencies, dataSourceName]
  );

  if (!avgLatency && !hasError) {
    return null;
  }

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
