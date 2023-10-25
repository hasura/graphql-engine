import { LogicalModel, Source } from '../../../hasura-metadata-types';

export type LogicalModelWithSourceName = LogicalModel & {
  source: Source;
};

export type Permission = {
  roleName: string;
  source: string;
  action: Action;
  filter: Record<string, any>;
  columns: string[];
  isNew: boolean;
};

export type PermissionId = {
  source: Permission['source'];
  name: Permission['roleName'];
};

export type Action = 'select' | 'insert' | 'update' | 'delete';

export type LogicalModelWithPermissions = LogicalModelWithSourceName & {
  select_permissions?: {
    role: string;
    permission: {
      columns: string[];
      filter: Record<string, any>;
    };
  }[];
};

export type Role = {
  name: string;
  isNew?: boolean;
};

export type AccessType = 'fullAccess' | 'noAccess' | 'partialAccess';

export type OnSave = (permission: Permission) => Promise<void>;

export type OnDelete = (permission: Permission) => Promise<void>;

export type RowSelectPermissionsType = 'with_custom_filter' | 'without_filter';
