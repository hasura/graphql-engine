import { allowedMetadataTypes } from '../../../MetadataAPI';

import { AccessType, QueryType } from '../../types';
import { PermissionsSchema } from '../../schema';
import { createInsertArgs, ExistingPermission } from './utils';
import { Table } from '../../../hasura-metadata-types';

interface CreateBodyArgs {
  dataSourceName: string;
  table: Table;
  role: string;
  resourceVersion: number;
}

interface CreateDeleteBodyArgs extends CreateBodyArgs {
  queries: QueryType[];
  driver: string;
}

const createDeleteBody = ({
  driver,
  dataSourceName,
  table,
  role,
  resourceVersion,
  queries,
}: CreateDeleteBodyArgs): {
  type: allowedMetadataTypes;
  source: string;
  resource_version: number;
  args: BulkArgs[];
} => {
  const args = queries.map(queryType => ({
    type: `${driver}_drop_${queryType}_permission` as allowedMetadataTypes,
    args: {
      table,
      role,
      source: dataSourceName,
    },
  }));

  const body = {
    type: 'bulk' as allowedMetadataTypes,
    source: 'default',
    resource_version: resourceVersion,
    args,
  };

  return body;
};

interface CreateBulkDeleteBodyArgs {
  dataSourceName: string;
  table: unknown;
  resourceVersion: number;
  roleList?: Array<{ roleName: string; queries: string[] }>;
  driver: string;
}

interface BulkArgs {
  type: allowedMetadataTypes;
  args: Record<string, string | allowedMetadataTypes | unknown>;
}

const createBulkDeleteBody = ({
  dataSourceName,
  driver,
  table,
  resourceVersion,
  roleList,
}: CreateBulkDeleteBodyArgs): {
  type: allowedMetadataTypes;
  source: string;
  resource_version: number;
  args: BulkArgs[];
} => {
  const args =
    roleList?.reduce<BulkArgs[]>((acc, role) => {
      role.queries.forEach(queryType => {
        acc.push({
          type: `${driver}_drop_${queryType}_permission` as allowedMetadataTypes,
          args: {
            table,
            role: role.roleName,
            source: dataSourceName,
          },
        });
      });

      return acc;
    }, []) || [];

  const body = {
    type: 'bulk' as allowedMetadataTypes,
    source: dataSourceName,
    resource_version: resourceVersion,
    args: args ?? [],
  };

  return body;
};

interface CreateInsertBodyArgs extends CreateBodyArgs {
  queryType: QueryType;
  formData: PermissionsSchema;
  accessType: AccessType;
  existingPermissions: ExistingPermission[];
  driver: string;
  tables: Table[];
  dataSourceName: string;
  table: Table;
  role: string;
}

export interface InsertBodyResult {
  type: allowedMetadataTypes;
  resource_version: number;
  args: Record<string, any>[];
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
  tables,
}: CreateInsertBodyArgs): InsertBodyResult => {
  const args = createInsertArgs({
    driver,
    dataSourceName,
    table,
    queryType,
    role,
    formData,
    accessType,
    existingPermissions,
    tables,
  });

  const formBody = {
    type: 'bulk' as allowedMetadataTypes,
    resource_version: resourceVersion,
    args: args ?? [],
  };

  return formBody;
};

export const api = {
  createInsertBody,
  createDeleteBody,
  createBulkDeleteBody,
};
