import { PermissionsSchema } from '../../schema';
import {
  AccessType,
  DataQueryType,
  Table,
  TablePermissionValidateInput,
} from '@hasura/shared/types';
import { inputValidationSchema } from '../components/InputValidation/InputValidation';
import { z } from 'zod';
import { areTablesEqual, getDriverPrefix } from '@hasura/metadata/helpers';
import { TMigrationSingleQuery } from '@hasura/metadata/api';
import { transformHeaderConfigs } from '@hasura/shared/utils';

const formatFilterValues = (formFilter: Record<string, any>[] = []) => {
  return Object.entries(formFilter).reduce<Record<string, any>>(
    (acc, [operator, value]) => {
      if (operator === '_and' || operator === '_or') {
        const filteredEmptyObjects = (value as any[]).filter(
          (p) => Object.keys(p).length !== 0,
        );
        acc[operator] = filteredEmptyObjects;
        return acc;
      }

      acc[operator] = value;
      return acc;
    },
    {},
  );
};

type SelectPermissionMetadata = {
  columns: string[];
  computed_fields: string[];
  set: Record<string, any>;
  filter: Record<string, any>;
  allow_aggregations?: boolean;
  limit?: number;
  query_root_fields?: string[];
  subscription_root_fields?: string[];
};

const createSelectObject = (input: PermissionsSchema) => {
  if (input.queryType === 'select') {
    const columns = Object.entries(input.columns)
      .filter(({ 1: value }) => value)
      .map(([key]) => key);
    const computed_fields = Object.entries(input.computed_fields)
      .filter(({ 1: value }) => value)
      .map(([key]) => key);

    // Input may be undefined
    const filter = formatFilterValues(input.filter);

    const permissionObject: SelectPermissionMetadata = {
      columns,
      computed_fields,
      filter,
      set: {},
      allow_aggregations: input.aggregationEnabled,
    };

    if (input.customRootFieldEnabled) {
      if (input.query_root_fields) {
        permissionObject.query_root_fields = input.query_root_fields;
      }
      if (input.subscription_root_fields) {
        permissionObject.subscription_root_fields =
          input.subscription_root_fields;
      }
    }

    if (input.rowCount || input.rowCount === '0') {
      permissionObject.limit = parseInt(input.rowCount, 10);
    }

    return permissionObject;
  }

  throw new Error('Case not handled');
};

const createValidateInputObject = (
  input: PermissionsSchema,
): TablePermissionValidateInput | undefined => {
  if (!input.validateInput?.enabled) return undefined;

  const { enabled, definition, ...others } = input.validateInput;
  return {
    ...others,
    definition: {
      ...definition,
      forward_client_headers: definition.forward_client_headers ?? false,
      headers: transformHeaderConfigs(definition.headers),
      timeout:
        typeof definition.timeout === 'string'
          ? Number.parseInt(definition.timeout)
          : (definition.timeout ?? 10),
    },
  };
};

type InsertPermissionMetadata = {
  columns: string[];
  check: Record<string, any>;
  allow_upsert: boolean;
  backend_only?: boolean;
  set: Record<string, any>;
  validate_input?: TablePermissionValidateInput;
};

const createInsertObject = (input: PermissionsSchema) => {
  if (input.queryType === 'insert') {
    const columns = Object.entries(input.columns)
      .filter(({ 1: value }) => value)
      .map(([key]) => key);

    const set =
      input?.presets?.reduce((acc, preset) => {
        if (preset.columnName === 'default') return acc;
        return { ...acc, [preset.columnName]: preset.columnValue };
      }, {}) ?? {};

    const permissionObject: InsertPermissionMetadata = {
      columns,
      check: input.check,
      allow_upsert: true,
      set,
      backend_only: input.backendOnly,
    };

    const validateInput = createValidateInputObject(input);
    if (validateInput) {
      permissionObject.validate_input = validateInput;
    }

    return permissionObject;
  }

  throw new Error('Case not handled');
};

export type DeletePermissionMetadata = {
  columns?: string[];
  set?: Record<string, any>;
  backend_only: boolean;
  filter: Record<string, any>;
  validate_input?: TablePermissionValidateInput;
};

const createDeleteObject = (input: PermissionsSchema) => {
  if (input.queryType === 'delete') {
    // Input may be undefined
    const filter = formatFilterValues(input.filter);

    const permissionObject: DeletePermissionMetadata = {
      backend_only: input.backendOnly || false,
      filter,
    };

    const validateInput = createValidateInputObject(input);
    if (validateInput) {
      permissionObject.validate_input = validateInput;
    }

    return permissionObject;
  }

  throw new Error('Case not handled');
};

type UpdatePermissionMetadata = {
  columns: string[];
  filter: Record<string, any>; // filter is PRE
  check?: Record<string, any>; // check is POST
  backend_only?: boolean;
  set: Record<string, any>;
  validate_input?: z.infer<typeof inputValidationSchema>;
};

const createUpdateObject = (input: PermissionsSchema) => {
  if (input.queryType === 'update') {
    const columns = Object.entries(input.columns)
      .filter(({ 1: value }) => value)
      .map(([key]) => key);

    const filter = formatFilterValues(input.filter);

    const check = formatFilterValues(input.check);
    const set =
      input?.presets?.reduce((acc, preset) => {
        if (preset.columnName === 'default') return acc;
        const isNumber = !isNaN(Number(preset.columnValue));
        return {
          ...acc,
          [preset.columnName]: isNumber
            ? Number(preset.columnValue)
            : preset.columnValue,
        };
      }, {}) ?? {};

    const permissionObject: UpdatePermissionMetadata = {
      columns,
      filter,
      check,
      set,
      backend_only: input.backendOnly,
      validate_input: input.validateInput,
    };

    return permissionObject;
  }

  throw new Error('Case not handled');
};

/**
 * creates the permissions object for the server
 */
const createPermission = (formData: PermissionsSchema) => {
  switch (formData.queryType) {
    case 'select':
      return createSelectObject(formData);
    case 'insert':
      return createInsertObject(formData);
    case 'update':
      return createUpdateObject(formData);
    case 'delete':
      return createDeleteObject(formData);
    default:
      throw new Error('Case not handled');
  }
};

export interface CreateInsertArgs {
  dataSourceName: string;
  table: unknown;
  queryType: DataQueryType;
  role: string;
  accessType: AccessType;
  formData: PermissionsSchema;
  existingPermissions: ExistingPermission[];
  driver: string;
}

export interface ExistingPermission {
  table: unknown;
  role: string;
  queryType: string;
}

/**
 * creates the insert arguments to update permissions
 * and creates drop arguments where permissions already exist
 */
export const createInsertArgs = ({
  dataSourceName,
  table,
  queryType,
  role,
  formData,
  existingPermissions,
  driver,
}: CreateInsertArgs): TMigrationSingleQuery[] => {
  const permission = createPermission(formData);
  const prefix = getDriverPrefix(driver);
  // create args object with args from form
  const args: TMigrationSingleQuery[] = [
    {
      type: `${prefix}_create_${queryType}_permission` as const,
      args: {
        table,
        role,
        permission,
        source: dataSourceName,
        comment: formData.comment,
      },
    },
  ];

  // determine if args from form already exist
  const permissionExists = existingPermissions.find(
    (existingPermission) =>
      areTablesEqual(existingPermission.table as Table, table as Table) &&
      existingPermission.role === role &&
      existingPermission.queryType === queryType,
  );

  // if the permission already exists it needs to be dropped
  if (permissionExists) {
    args.unshift({
      type: `${prefix}_drop_${queryType}_permission` as const,
      args: {
        table,
        role,
        source: dataSourceName,
      },
    });
  }

  return args;
};
