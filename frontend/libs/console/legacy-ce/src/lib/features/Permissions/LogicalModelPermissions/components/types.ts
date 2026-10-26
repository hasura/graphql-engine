import { LogicalModelWithSource } from '@hasura/metadata/helpers';

export type Permission = {
  roleName: string;
  source: string;
  action: Action;
  filter: Record<string, any>;
  columns: string[];
  isNew: boolean;
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
export type OnSave = (permission: Permission) => Promise<void>;

export type OnDelete = (permission: Permission) => Promise<void>;

export type RowSelectPermissionsType = 'with_custom_filter' | 'without_filter';
