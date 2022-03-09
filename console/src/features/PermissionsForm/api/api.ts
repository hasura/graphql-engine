import {
  DataTarget,
  isPostgresDataTarget,
  isMSSQLDataTarget,
} from '@/features/Datasources';
import { allowedMetadataTypes } from '@/features/MetadataAPI';
import { MetadataDataSource, QualifiedTable } from '@/metadata/types';
import { AccessType, FormOutput, QueryType } from '../types';

const driverPrefixes = {
  postgres: 'pg',
  mysql: 'mysql',
  mssql: 'mssql',
  bigquery: 'bigquery',
  citus: 'citus',
};

interface CreateBodyArgs {
  driver: MetadataDataSource['kind'];
  dataTarget: DataTarget;
  roleName: string;
  resourceVersion: number;
}

interface CreateDeleteBodyArgs extends CreateBodyArgs {
  queries: QueryType[];
}

const createDeleteBody = ({
  driver,
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
  const driverPrefix = driverPrefixes[driver];

  if (!isPostgresDataTarget(dataTarget) || !isMSSQLDataTarget(dataTarget)) {
    throw new Error(`${driver} not supported`);
  }

  const args = queries.map(queryType => ({
    type: `${driverPrefix}_drop_${queryType}_permission` as allowedMetadataTypes,
    args: {
      table: { schema: dataTarget.schema, name: dataTarget.table },
      role: roleName,
      source: dataTarget.database,
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
  args: Record<string, string | allowedMetadataTypes | QualifiedTable>;
}

const createBulkDeleteBody = ({
  driver,
  dataTarget,
  resourceVersion,
  roleList,
}: CreateBulkDeleteBodyArgs): {
  type: allowedMetadataTypes;
  source: string;
  resource_version: number;
  args: BulkArgs[];
} => {
  const driverPrefix = driverPrefixes[driver];

  if (!isPostgresDataTarget(dataTarget) || !isMSSQLDataTarget(dataTarget)) {
    throw new Error('Only Postgres, Citus and MsSql is supported');
  }

  const args =
    roleList?.reduce<BulkArgs[]>((acc, role) => {
      role.queries.forEach(queryType => {
        acc.push({
          type: `${driverPrefix}_drop_${queryType}_permission` as allowedMetadataTypes,
          args: {
            table: { schema: dataTarget.schema, name: dataTarget.table },
            role: role.roleName,
            source: dataTarget.database,
          },
        });
      });

      return acc;
    }, []) || [];

  const body = {
    type: 'bulk' as allowedMetadataTypes,
    source: dataTarget.database,
    resource_version: resourceVersion,
    args: args ?? [],
  };

  return body;
};

interface CreateInsertBodyArgs extends CreateBodyArgs {
  queryType: QueryType;
  formData: FormOutput;
  accessType: AccessType;
}

const createInsertBody = ({
  driver,
  dataTarget,
  queryType,
  roleName,
  formData,
  accessType,
  resourceVersion,
}: CreateInsertBodyArgs): {
  type: allowedMetadataTypes;
  resource_version: number;
  args: Record<string, any>[];
} => {
  const driverPrefix = driverPrefixes[driver];

  if (!isPostgresDataTarget(dataTarget) || !isMSSQLDataTarget(dataTarget)) {
    throw new Error(`${driver} not supported`);
  }

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

  const args: Record<string, any>[] = [
    {
      type: `${driverPrefix}_create_${queryType}_permission` as allowedMetadataTypes,
      args: {
        table: { schema: dataTarget.schema, name: dataTarget.table },
        role: roleName,
        permission,
        source: dataTarget.database,
      },
    },
  ];

  if (accessType !== 'noAccess') {
    args.unshift({
      type: `${driverPrefix}_drop_${queryType}_permission` as allowedMetadataTypes,
      args: {
        table: { schema: dataTarget.schema, name: dataTarget.table },
        role: roleName,
        source: dataTarget.database,
      },
    } as typeof args[0]);
  }

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
