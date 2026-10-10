import { useState } from 'react';

export type PermissionEdit = {
  newRole: string;
  isNewRole: boolean;
  isNewPerm: boolean;
  role: string;
  filter?: Record<string, string>;
};

const createPermissionEditState = (): PermissionEdit => ({
  newRole: '',
  isNewRole: false,
  isNewPerm: false,
  role: '',
});

const useRemoteSchemaRelationshipForm = () => {
  const [bulkSelect, setBulkSelect] = useState<string[]>([]);
  const [isEditing, setIsEditing] = useState(false);
  const [schemaDefinition, setSchemaDefinition] = useState('');
  const [permissionEdit, setPermissionEdit] = useState(
    createPermissionEditState(),
  );

  const permSetBulkSelect = (isAdd: boolean, role: string) => {
    setBulkSelect((prev) => {
      return isAdd ? [...prev, role] : prev.filter((e) => e !== role);
    });
  };

  const permSetRoleName = (role: string) => {
    setPermissionEdit((prev) => ({
      ...prev,
      role,
    }));
  };

  const permOpenEdit = (
    role: string,
    isNewRole: boolean,
    isNewPerm: boolean,
  ) => {
    setIsEditing(true);
    setPermissionEdit((prev) => ({
      ...prev,
      role,
      isNewRole,
      isNewPerm,
      filter: {},
    }));
  };

  const permCloseEdit = () => {
    setIsEditing(false);
    setPermissionEdit(createPermissionEditState());
  };

  return {
    bulkSelect,
    permSetBulkSelect,
    permissionEdit,
    permSetRoleName,
    permOpenEdit,
    isEditing,
    permCloseEdit,
    schemaDefinition,
    setSchemaDefinition,
    setBulkSelect,
  };
};

export default useRemoteSchemaRelationshipForm;
