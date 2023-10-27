import { z } from 'zod';
import { inputValidationSchema } from '../../../components/Services/Data/TablePermissions/InputValidation/InputValidation';

export type Permission =
  | InsertPermission
  | SelectPermission
  | UpdatePermission
  | DeletePermission;

type BasePermission = {
  role: string;
  comment?: string;
};

export interface InsertPermission extends BasePermission {
  permission: InsertPermissionDefinition;
}
export interface InsertPermissionDefinition {
  check?: Record<string, unknown>;
  filter?: Record<string, unknown>;
  set?: Record<string, unknown>;
  columns?: string[];
  backend_only?: boolean;
  comment?: string;
  validate_input?: z.infer<typeof inputValidationSchema>;
}

export interface SelectPermission extends BasePermission {
  permission: SelectPermissionDefinition;
}
export interface SelectPermissionDefinition {
  columns?: string[];
  filter?: Record<string, unknown>;
  allow_aggregations?: boolean;
  query_root_fields?: string[] | null;
  subscription_root_fields?: string[] | null;
  limit?: number;
  comment?: string;
}

export interface UpdatePermission extends BasePermission {
  permission: UpdatePermissionDefinition;
}

export interface UpdatePermissionDefinition {
  columns?: string[];
  filter?: Record<string, unknown>;
  check?: Record<string, unknown>;
  set?: Record<string, unknown>;
  backend_only?: boolean;
  comment?: string;
  validate_input?: z.infer<typeof inputValidationSchema>;
}

export interface DeletePermission extends BasePermission {
  permission: DeletePermissionDefinition;
}
export interface DeletePermissionDefinition {
  filter?: Record<string, unknown>;
  backend_only?: boolean;
  validate_input?: z.infer<typeof inputValidationSchema>;
}
