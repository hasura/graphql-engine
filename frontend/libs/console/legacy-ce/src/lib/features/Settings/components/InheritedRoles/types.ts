import { InheritedRole } from '@hasura/shared/types';

export interface RoleActionsInterface {
  inheritedRoleName: string;
  onEdit: (inhertitedRole: InheritedRole) => void;
  onDelete: (inhertitedRole: InheritedRole) => void;
  onAdd: (inheritedRoleName: string) => void;
  onRoleNameChange: (InheritedRoleName: string) => void;
}
