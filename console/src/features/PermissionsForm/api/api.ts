import { Api } from '@/hooks/apiUtils';
import Endpoints from '@/Endpoints';

import { QualifiedTable } from '@/metadata/types';
import { AccessType, FormOutput, QueryType } from '../types';

interface CreateBodyArgs {
  dataSource: string;
  qualifiedTable: QualifiedTable;
  roleName: string;
  resourceVersion: number;
}

interface CreateDeleteBodyArgs extends CreateBodyArgs {
  queries: QueryType[];
}

const createDeleteBody = ({
  dataSource,
  qualifiedTable,
  roleName,
  resourceVersion,
  queries,
}: CreateDeleteBodyArgs) => {
  const args = queries.map(queryType => ({
    type: `${dataSource}_drop_${queryType}_permission`,
    args: {
      table: qualifiedTable,
      role: roleName,
      source: 'default',
    },
  }));

  const body = {
    type: 'bulk',
    source: 'default',
    resource_version: resourceVersion,
    args,
  };

  return body;
};

interface CreateInsertBodyArgs extends CreateBodyArgs {
  queryType: QueryType;
  formData: FormOutput;
  accessType: AccessType;
}

const createInsertBody = ({
  dataSource,
  qualifiedTable,
  queryType,
  roleName,
  formData,
  accessType,
  resourceVersion,
}: CreateInsertBodyArgs) => {
  const presets = formData.presets?.reduce((acc, preset) => {
    if (preset.columnValue) {
      acc[preset.columnName] = preset.columnValue;
    }

    return acc;
  }, {} as Record<string, string | number>);

  const columns = Object.entries(formData.columns)
    .filter(({ 1: value }) => value)
    .map(([key]) => key);

  const permission = {
    columns,
    presets,
    computed_fields: [],
    backend_only: formData.backendOnly,
    limit: parseInt(formData.rowCount, 10),
    allow_aggregations: formData.aggregationEnabled,
    check: JSON.parse(formData.check || '{}'),
    filter: JSON.parse(formData.filter || '{}'),
  };

  const args = [
    {
      type: `${dataSource}_create_${queryType}_permission`,
      args: {
        table: qualifiedTable,
        role: roleName,
        permission,
        source: 'default',
      },
    },
  ];

  if (accessType !== 'noAccess') {
    args.unshift({
      type: `${dataSource}_drop_${queryType}_permission`,
      args: {
        table: qualifiedTable,
        role: roleName,
        source: 'default',
      },
    } as typeof args[0]);
  }

  const formBody = {
    type: 'bulk',
    source: 'default',
    resource_version: resourceVersion,
    args,
  };

  return formBody;
};

interface CreatePermissionsArgs {
  headers: any;
  body: ReturnType<typeof createInsertBody>;
}

const createPermissions = ({ headers, body }: CreatePermissionsArgs) =>
  Api.post<string[]>(
    { headers, body, url: Endpoints.metadata },
    result => result
  );

interface DeletePermissionsArgs {
  headers: any;
  body: ReturnType<typeof createDeleteBody>;
}

const deletePermissions = ({ headers, body }: DeletePermissionsArgs) =>
  Api.post<string[]>(
    { headers, body, url: Endpoints.metadata },
    result => result
  );

export const api = {
  createPermissions,
  deletePermissions,
  createInsertBody,
  createDeleteBody,
};
