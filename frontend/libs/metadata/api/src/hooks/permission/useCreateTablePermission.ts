import type {
  DataQueryType,
  SupportedDriver,
  Table,
  TablePermissionDefinition,
} from '@hasura/shared/types';
import { getDriverPrefix } from '@hasura/metadata/helpers';
import { TMigrationSingleQuery } from '../../api';

export const getCreatePermissionQuery = (
  action: DataQueryType,
  driver: SupportedDriver,
  args: {
    comment?: string;
    table: Table;
    role: string;
    source: string;
    permission: TablePermissionDefinition;
  },
): TMigrationSingleQuery => {
  const prefix = getDriverPrefix(driver);
  const queryType = `${prefix}_create_${action}_permission` as const;

  return {
    type: queryType,
    args,
  };
};
