import { getDriverPrefix } from '@hasura/metadata/helpers';
import { PermissionsSchema } from '../../schema';
import { createInsertArgs, ExistingPermission } from './utils';
import { AccessType, DataQueryType, Table } from '@hasura/shared/types';
import { TMigrationBulkQuery, TMigrationQuery } from '@hasura/metadata/api';

interface CreateBodyArgs {
  dataSourceName: string;
  table: Table;
  role: string;
  resourceVersion: number;
}

interface CreateDeleteBodyArgs extends CreateBodyArgs {
  queries: DataQueryType[];
  driver: string;
}

const createDeleteBody = ({
  driver,
  dataSourceName,
  table,
  role,
  resourceVersion,
  queries,
}: CreateDeleteBodyArgs): TMigrationBulkQuery => {
  const prefix = getDriverPrefix(driver);
  const args = queries.map((queryType) => ({
    type: `${prefix}_drop_${queryType}_permission` as const,
    args: {
      table,
      role,
      source: dataSourceName,
    },
  }));

  const body = {
    type: 'bulk' as const,
    resource_version: resourceVersion,
    args,
  };

  return body;
};

interface CreateInsertBodyArgs extends CreateBodyArgs {
  queryType: DataQueryType;
  formData: PermissionsSchema;
  accessType: AccessType;
  existingPermissions: ExistingPermission[];
  driver: string;
  dataSourceName: string;
  table: Table;
  role: string;
}

const createInsertBody = ({
  dataSourceName,
  table,
  queryType,
  role,
  formData,
  accessType,
  resourceVersion,
  existingPermissions,
  driver,
}: CreateInsertBodyArgs): TMigrationQuery => {
  const args = createInsertArgs({
    driver,
    dataSourceName,
    table,
    queryType,
    role,
    formData,
    accessType,
    existingPermissions,
  });

  const formBody = {
    type: 'bulk' as const,
    resource_version: resourceVersion,
    args: args ?? [],
  };

  return formBody;
};

export const api = {
  createInsertBody,
  createDeleteBody,
};
