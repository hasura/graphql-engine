import { Badge, Tooltip } from '@hasura/shared/ui';
import { FaExclamationTriangle } from 'react-icons/fa';
import { useServerConfig } from '@hasura/metadata/api';
import { Skeleton } from '@radix-ui/themes';

export const AllowListStatus = () => {
  const { data: configData, isLoading, isError } = useServerConfig();

  if (isError) {
    return (
      <Tooltip content="Status unknown. Config API is currently unavailable.">
        <Badge color="yellow">
          <FaExclamationTriangle />
        </Badge>
      </Tooltip>
    );
  }

  return (
    <Skeleton loading={isLoading}>
      {configData?.is_allow_list_enabled ? (
        <Badge color="green">Enabled</Badge>
      ) : (
        <Badge color="indigo">Disabled</Badge>
      )}{' '}
    </Skeleton>
  );
};
