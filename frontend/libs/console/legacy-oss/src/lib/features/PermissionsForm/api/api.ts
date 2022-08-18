import { allowedMetadataTypes } from '@/features/MetadataAPI';
import { NewDataTarget } from '../../PermissionsTab/types/types';

import { AccessType, FormOutput, QueryType } from '../types';
import { createInsertArgs, driverPrefixes } from './utils';

interface CreateBodyArgs {
  dataTarget: NewDataTarget;
  roleName: string;
  resourceVersion: number;
}

interface CreateDeleteBodyArgs extends CreateBodyArgs {
  queries: QueryType[];
}

const createDeleteBody = ({
  dataTarget,
  roleName,
  resourceVersion,
  queries,
}: CreateDeleteBodyArgs): {
  type: allowedMetadataTypes;
  source: string;
  resource_version: number;
  args: BulkArgs[];
} => {
  const driverPrefix = driverPrefixes[dataTarget.dataSource.driver];

  if (!['postgres', 'mssql'].includes(dataTarget.dataSource.driver)) {
    throw new Error(`${dataTarget.dataSource.driver} not supported`);
  }

  const args = queries.map(queryType => ({
    type: `${driverPrefix}_drop_${queryType}_permission` as allowedMetadataTypes,
    args: {
      table: dataTarget.dataLeaf.leaf?.name || '',
      role: roleName,
      source: dataTarget.dataSource.database,
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

interface CreateBulkDeleteBodyArgs extends CreateBodyArgs {
  roleList?: Array<{ roleName: string; queries: QueryType[] }>;
}

interface BulkArgs {
  type: allowedMetadataTypes;
  args: Record<string, string | allowedMetadataTypes>;
}

const createBulkDeleteBody = ({
  dataTarget,
  resourceVersion,
  roleList,
}: CreateBulkDeleteBodyArgs): {
  type: allowedMetadataTypes;
  source: string;
  resource_version: number;
  args: BulkArgs[];
} => {
  const driverPrefix = driverPrefixes[dataTarget.dataSource.driver];

  if (!['postgres', 'mssql'].includes(dataTarget.dataSource.driver)) {
    throw new Error(`${dataTarget.dataSource.driver} not supported`);
  }

  const args =
    roleList?.reduce<BulkArgs[]>((acc, role) => {
      role.queries.forEach(queryType => {
        acc.push({
          type: `${driverPrefix}_drop_${queryType}_permission` as allowedMetadataTypes,
          args: {
            table: dataTarget.dataLeaf.leaf?.name || '',
            role: role.roleName,
            source: dataTarget.dataSource.database,
          },
        });
      });

      return acc;
    }, []) || [];

  const body = {
    type: 'bulk' as allowedMetadataTypes,
    source: dataTarget.dataSource.database,
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

interface InsertBodyResult {
  type: allowedMetadataTypes;
  resource_version: number;
  args: Record<string, any>[];
}

const createInsertBody = ({
  dataTarget,
  queryType,
  roleName,
  formData,
  // accessType,
  resourceVersion,
  existingPermissions,
}: CreateInsertBodyArgs): InsertBodyResult => {
  const driverPrefix = driverPrefixes[dataTarget.dataSource.driver];

  if (!['postgres', 'mssql'].includes(dataTarget.dataSource.driver)) {
    throw new Error(`${dataTarget.dataSource.driver} not supported`);
  }

  const args = createInsertArgs({
    driverPrefix,
    database: dataTarget.dataSource.database,
    table: dataTarget.dataLeaf.leaf?.name || '',
    queryType,
    role: roleName,
    formData,
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
