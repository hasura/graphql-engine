import { InheritedRole } from '../../../../metadata/types';

export interface RoleActionsInterface {
  onEdit: (inhertitedRole: InheritedRole) => void;
  onDelete: (inhertitedRole: InheritedRole) => void;
  onAdd: (inheritedRoleName: string) => void;
  onRoleNameChange: (InheritedRoleName: string) => void;
}
