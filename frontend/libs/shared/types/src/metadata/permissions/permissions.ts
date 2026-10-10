import { HeaderConfig } from '../header';
import {
  QueryRootPermissionType,
  SubscriptionRootPermissionType,
} from './schema';

export type DataQueryType = 'update' | 'insert' | 'delete' | 'select';

export const DATA_QUERY_TYPES: DataQueryType[] = [
  'insert',
  'select',
  'update',
  'delete',
];

export type AccessType =
  'fullAccess' | 'noAccess' | 'partialAccess' | 'partialAccessWarning';

export type TablePermission =
  InsertPermission | SelectPermission | UpdatePermission | DeletePermission;

export type TablePermissionDefinition =
  | InsertPermissionDefinition
  | UpdatePermissionDefinition
  | SelectPermissionDefinition
  | DeletePermissionDefinition;

export type BasePermission = {
  role: string;
  comment?: string;
};

export interface InsertPermission extends BasePermission {
  permission: InsertPermissionDefinition;
}

/**
 * https://hasura.io/docs/2.0/api-reference/metadata-api/permission/#metadata-pg-create-insert-permission
 */
export interface InsertPermissionDefinition {
  /** This expression has to hold true for every new row that is inserted */
  check?: Record<string, unknown>;
  /** Preset values for columns that can be sourced from session variables or static values */
  set?: Record<string, unknown>;
  /** Can insert into only these columns (or all when '*' is specified) */
  columns?: string[] | '*';
  /**
   * When set to true the mutation is accessible only if x-hasura-use-backend-only-permissions session variable exists
   * and is set to true and request is made with x-hasura-admin-secret set if any auth is configured
   */
  backend_only?: boolean;
  comment?: string;
  validate_input?: TablePermissionValidateInput;
}

export interface SelectPermission extends BasePermission {
  permission: SelectPermissionDefinition;
}

export interface SelectPermissionDefinition {
  /** Only these columns are selectable (or all when '*' is specified) */
  columns?: string[] | '*';
  /** Only these computed fields are selectable */
  computed_fields?: string[];
  /** Only the rows where this precondition holds true are selectable */
  filter?: { [key: string]: unknown };
  /** Toggle allowing aggregate queries */
  allow_aggregations?: boolean;
  query_root_fields?: QueryRootPermissionType[] | null;
  subscription_root_fields?: SubscriptionRootPermissionType[] | null;
  limit?: number;
  comment?: string;
}

export interface UpdatePermission extends BasePermission {
  permission: UpdatePermissionDefinition;
}

export interface UpdatePermissionDefinition {
  /** Only these columns are selectable (or all when '*' is specified) */
  columns?: string[] | '*';
  /** Only the rows where this precondition holds true are updatable */
  filter?: { [key: string]: unknown };
  /** Postcondition which must be satisfied by rows which have been updated */
  check?: { [key: string]: unknown };
  /** Preset values for columns that can be sourced from session variables or static values */
  set?: Record<string, unknown>;
  backend_only?: boolean;
  comment?: string;
  validate_input?: TablePermissionValidateInput;
}

export interface DeletePermission extends BasePermission {
  permission: DeletePermissionDefinition;
}

export interface DeletePermissionDefinition {
  /** Only the rows where this precondition holds true are updatable */
  filter?: { [key: string]: unknown };
  backend_only?: boolean;
  validate_input?: TablePermissionValidateInput;
}

export interface TablePermissionValidateInput {
  type: 'http';
  definition: {
    url: string;
    timeout: number;
    headers: HeaderConfig[];
    forward_client_headers: boolean;
  };
}

export const permissionToKey = {
  insert: 'insert_permissions',
  select: 'select_permissions',
  update: 'update_permissions',
  delete: 'delete_permissions',
} as const;

export const metadataPermissionKeys = [
  'insert_permissions',
  'select_permissions',
  'update_permissions',
  'delete_permissions',
] as const;

export type MetadataPermissionKey = (typeof metadataPermissionKeys)[number];

export const keyToPermission = {
  insert_permissions: 'insert',
  select_permissions: 'select',
  update_permissions: 'update',
  delete_permissions: 'delete',
} as const;
