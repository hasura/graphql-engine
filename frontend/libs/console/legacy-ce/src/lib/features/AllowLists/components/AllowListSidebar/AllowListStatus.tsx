import Skeleton from 'react-loading-skeleton';
import { useServerConfig } from '../../../../hooks';
import { Badge } from '../../../../new-components/Badge';
import { Tooltip } from '../../../../new-components/Tooltip';
import React from 'react';
import { FaExclamationTriangle } from 'react-icons/fa';

export const AllowListStatus = () => {
  const { data: configData, isLoading, isError } = useServerConfig();

  if (isLoading) {
    return <Skeleton width={80} height={20} />;
  }

  if (isError) {
    return (
      <Tooltip tooltipContentChildren="Status unknown. Config API is currently unavailable.">
        <Badge color="yellow" className="-ml-1 p-0">
          <FaExclamationTriangle />
        </Badge>
      </Tooltip>
    );
  }

  if (configData?.is_allow_list_enabled) {
    return <Badge color="green">Enabled</Badge>;
  }

  return <Badge color="indigo">Disabled</Badge>;
};
