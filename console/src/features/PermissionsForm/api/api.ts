import { allowedMetadataTypes } from '@/features/MetadataAPI';

import { AccessType, FormOutput, QueryType } from '../types';
import { createInsertArgs } from './utils';

interface CreateBodyArgs {
  currentSource: string;
  dataSourceName: string;
  table: unknown;
  roleName: string;
  resourceVersion: number;
}

interface CreateDeleteBodyArgs extends CreateBodyArgs {
  queries: QueryType[];
}

const createDeleteBody = ({
  currentSource,
  dataSourceName,
  table,
  roleName,
  resourceVersion,
  queries,
}: CreateDeleteBodyArgs): {
  type: allowedMetadataTypes;
  source: string;
  resource_version: number;
  args: BulkArgs[];
} => {
  // if (!['postgres', 'mssql'].includes(currentSource)) {
  //   throw new Error(`${currentSource} not supported`);
  // }

  const args = queries.map(queryType => ({
    type: `${currentSource}_drop_${queryType}_permission` as allowedMetadataTypes,
    args: {
      table,
      role: roleName,
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
  source: string;
  dataSourceName: string;
  table: unknown;
  resourceVersion: number;
  roleList?: Array<{ roleName: string; queries: string[] }>;
}

interface BulkArgs {
  type: allowedMetadataTypes;
  args: Record<string, string | allowedMetadataTypes | unknown>;
}

const createBulkDeleteBody = ({
  source,
  dataSourceName,
  table,
  resourceVersion,
  roleList,
}: CreateBulkDeleteBodyArgs): {
  type: allowedMetadataTypes;
  source: string;
  resource_version: number;
  args: BulkArgs[];
} => {
  // if (!['postgres', 'mssql'].includes(source)) {
  //   throw new Error(`${dataSourceName} not supported`);
  // }

  const args =
    roleList?.reduce<BulkArgs[]>((acc, role) => {
      role.queries.forEach(queryType => {
        acc.push({
          type: `${source}_drop_${queryType}_permission` as allowedMetadataTypes,
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
  formData: FormOutput;
  accessType: AccessType;
  existingPermissions: any;
}

export interface InsertBodyResult {
  type: allowedMetadataTypes;
  resource_version: number;
  args: Record<string, any>[];
}

const createInsertBody = ({
  currentSource,
  dataSourceName,
  table,
  queryType,
  roleName,
  formData,
  accessType,
  resourceVersion,
  existingPermissions,
}: CreateInsertBodyArgs): InsertBodyResult => {
  // if (!['postgres', 'mssql'].includes(currentSource)) {
  //   throw new Error(`${currentSource} not supported`);
  // }

  const args = createInsertArgs({
    currentSource,
    dataSourceName,
    table,
    queryType,
    role: roleName,
    formData,
    accessType,
    existingPermissions,
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
