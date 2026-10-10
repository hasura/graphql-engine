import React, { useEffect, useState } from 'react';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import InheritedRolesTable from './InheritedRolesTable';
import { RoleActionsInterface } from './types';
import InheritedRolesEditor from './InheritedRolesEditor';
import { getConfirmation } from '@hasura/shared/utils';
import {
  useInconsistentMetadata,
  useMetadata,
  useDropInheritedRole,
  useUpdateInheritedRole,
  useAddInheritedRole,
} from '@hasura/metadata/api';
import { useNavigate } from 'react-router';
import { METADATA_STATUS_PATH } from '@hasura/shared/types';
import {
  InconsistentObjectInheritedRole,
  InheritedRole,
} from '@hasura/shared/types';
import { SkeletonList, Text } from '@hasura/shared/ui';
import { MetadataSelectors } from '@hasura/metadata/helpers';
import { Flex, Heading } from '@radix-ui/themes';

export const ActionContext = React.createContext<RoleActionsInterface>(
  {} as RoleActionsInterface,
);

const InheritedRoles: React.FC = () => {
  const navigate = useNavigate();
  const deleteInheritedRole = useDropInheritedRole();
  const addInheritedRole = useAddInheritedRole();
  const updateInheritedRole = useUpdateInheritedRole();

  const { data: meta, isLoading: metadataLoading } = useMetadata();
  const {
    data: inconsistentMetadata,
    refetch: refetchInconsistentMetadata,
    isLoading: inconsistentMetadataLoading,
  } = useInconsistentMetadata();
  const allRoles = MetadataSelectors.getRoles(meta?.metadata);
  const inheritedRoles = meta?.metadata.inherited_roles ?? [];
  const inconsistentInheritedRoles: InconsistentObjectInheritedRole[] =
    inconsistentMetadata?.inconsistent_objects?.filter(
      (io) =>
        'type' in io && io.type === 'inherited role permission inconsistency',
    ) ?? [];

  const [inheritedRoleName, setInheritedRoleName] = useState('');
  const [inheritedRole, setInheritedRole] = useState<InheritedRole | null>(
    null,
  );
  const [isCollapsed, setIsCollapsed] = useState(true);

  const setEditorState = (
    roleName: string,
    role: InheritedRole | null,
    collapsed: boolean,
  ) => {
    setIsCollapsed(collapsed);
    setInheritedRole(role);
    setInheritedRoleName(roleName);
  };

  const onAdd = (roleName: string) => {
    setEditorState(roleName, null, false);
  };

  const onRoleNameChange = (roleName: string) => {
    setEditorState(roleName, null, isCollapsed);
  };

  const onEdit = (role: InheritedRole) => {
    setEditorState('', role, false);
  };

  const resetState = () => {
    setEditorState('', null, true);
  };

  const onDelete = (role: InheritedRole) => {
    const confirmMessage = `This will delete the inherited role "${role?.role_name}"`;
    const isOk = getConfirmation(confirmMessage);
    if (isOk) {
      deleteInheritedRole(role.role_name).finally(() => {
        refetchInconsistentMetadata();
      });
    }
  };

  const onSave = async (role: InheritedRole) => {
    const onComplete = () => {
      refetchInconsistentMetadata();
      resetState();
    };
    if (inheritedRole) {
      updateInheritedRole(role.role_name, role.role_set).finally(onComplete);
    } else {
      addInheritedRole(role.role_name, role.role_set).finally(onComplete);
    }
  };

  useEffect(() => {
    if (inconsistentInheritedRoles?.length) {
      // redirection will happen when there is an inconsitency detected for the first time
      // second time inherited roles page will load as expected
      // whenever there is an updation/ creation cause the inconsistency, it will redirect to status page once
      navigate(METADATA_STATUS_PATH);
    }
  }, [inconsistentInheritedRoles]);

  const isFetching =
    (!meta || !inconsistentMetadata) &&
    (metadataLoading || inconsistentMetadataLoading);

  return (
    <Analytics name="InheritedRoles" {...REDACT_EVERYTHING}>
      <Flex direction="column" gap="4" className="p-4">
        <Heading size="6" color="gray">
          Inherited Roles
        </Heading>
        <Text as="p">
          Inherited roles will combine the permissions of 2 or more roles.
        </Text>
        <ActionContext.Provider
          value={{
            onEdit,
            onAdd,
            onDelete,
            onRoleNameChange,
            inheritedRoleName,
          }}
        >
          <InheritedRolesTable inheritedRoles={inheritedRoles} />
        </ActionContext.Provider>

        {isFetching ? (
          <SkeletonList count={8} />
        ) : (
          <InheritedRolesEditor
            allRoles={allRoles}
            cancelCb={resetState}
            isCollapsed={isCollapsed}
            onSave={onSave}
            inheritedRoleName={inheritedRoleName}
            inheritedRole={inheritedRole}
          />
        )}
      </Flex>
    </Analytics>
  );
};

export default InheritedRoles;
