import { LogicalModelWithSource } from '@hasura/metadata/helpers';

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

export type Action = 'select';

export type LogicalModelWithPermissions = LogicalModelWithSource & {
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

export type OnSave = (permission: Permission) => Promise<void>;

export type OnDelete = (permission: Permission) => Promise<void>;

export type RowSelectPermissionsType = 'with_custom_filter' | 'without_filter';
