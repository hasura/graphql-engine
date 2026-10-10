import React, { useEffect, useState } from 'react';
import { GraphQLSchema } from 'graphql';
import PermissionsTable from './PermissionsTable';
import PermissionEditor from './PermissionEditor';
import { getRemoteSchemaFields, buildSchemaFromRoleDefn } from './utils';
import { RemoteSchemaFields } from './types';
import BulkSelect from './BulkSelect';
import { InconsistentBadge } from '../InconsistentBadge';
import { useDocumentTitle } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { RemoteSchema } from '@hasura/shared/types';
import {
  useInconsistentMetadata,
  useSaveRemoteSchemaPermission,
  useDropRemoteSchemaPermissions,
  useDropRemoteSchemaPermissionMultipleRoles,
  useIntrospectRemoteSchema,
} from '@hasura/metadata/api';
import useRemoteSchemaRelationshipForm from '../../hooks/useRemoteSchemaRelationshipForm';
import { findInconsistentRemoteSchema } from '@hasura/metadata/helpers';

export type PermissionsProps = {
  allRoles: string[];
  currentRemoteSchema: RemoteSchema;
};

const Permissions: React.FC<PermissionsProps> = ({
  allRoles,
  currentRemoteSchema,
}) => {
  useDocumentTitle(
    `Permissions - ${currentRemoteSchema.name} - Remote Schemas | Hasura`,
  );

  const { readOnlyMode } = useAppContext();
  const [remoteSchemaFields, setRemoteSchemaFields] = useState<
    RemoteSchemaFields[]
  >([]);
  const { data: inconsistentMetadata } = useInconsistentMetadata();
  const { mutate: saveRemoteSchemaPermission, isPending: isSaving } =
    useSaveRemoteSchemaPermission();
  const { mutate: removeRemoteSchemaPermission, isPending: isRemoving } =
    useDropRemoteSchemaPermissions();
  const { mutate: removePermissionMultipleRoles } =
    useDropRemoteSchemaPermissionMultipleRoles();
  const {
    data: schema,
    error,
    refetch: introspect,
  } = useIntrospectRemoteSchema(currentRemoteSchema.name);

  const {
    bulkSelect,
    permSetBulkSelect,
    permissionEdit,
    permSetRoleName,
    permOpenEdit,
    permCloseEdit,
    isEditing,
    schemaDefinition,
    setSchemaDefinition,
    setBulkSelect,
  } = useRemoteSchemaRelationshipForm();

  useEffect(() => {
    if (!schema) return;
    const isNewRole: boolean = permissionEdit.isNewRole;
    let permissionsSchema: GraphQLSchema | null = null;

    if (!isNewRole && !!schemaDefinition) {
      permissionsSchema = buildSchemaFromRoleDefn(schemaDefinition);
    }

    // when server throws error while saving new role, do not reset the remoteSchemaFields
    // persist the user defined schema in th UI
    if (isNewRole && schemaDefinition) return;

    if (schema)
      setRemoteSchemaFields(getRemoteSchemaFields(schema, permissionsSchema));
  }, [schema, permissionEdit.isNewRole, schemaDefinition]);

  const inconsistencyDetails = findInconsistentRemoteSchema(
    inconsistentMetadata?.inconsistent_objects,
    currentRemoteSchema.name,
  );
  const handleSaveRemoteSchemaPermission = (
    successCb?: () => void,
    errorCb?: () => void,
  ) => {
    return saveRemoteSchemaPermission(
      {
        currentRemoteSchema,
        newRole: permissionEdit.newRole,
        role: permissionEdit.role,
        schemaDefinition,
      },
      successCb,
      errorCb,
    );
  };

  const handlerRemoveRemoteSchemaPermission = (
    successCb?: () => void,
    errorCb?: () => void,
  ) => {
    return removeRemoteSchemaPermission(
      {
        remoteSchemaName: currentRemoteSchema.name,
        role: permissionEdit.role,
      },
      successCb,
      errorCb,
    );
  };

  const permRemoveMultipleRoles = () => {
    removePermissionMultipleRoles(
      {
        currentRemoteSchema,
        roles: bulkSelect,
      },
      () => {
        permSetRoleName('');
        permCloseEdit();
        setBulkSelect([]);
      },
    );
  };

  if (error || !schema) {
    return (
      <>
        {inconsistencyDetails && (
          <InconsistentBadge inconsistencyDetails={inconsistencyDetails} />
        )}
        <div>
          Error introspecting remote schema.{' '}
          <a
            onClick={() => introspect()}
            className="cursor-pointer"
            role="button"
          >
            {' '}
            Try again{' '}
          </a>
        </div>
      </>
    );
  }

  const isFetching = isSaving || isRemoving;

  return (
    <div className="bootstrap-jail">
      <PermissionsTable
        allRoles={allRoles}
        currentRemoteSchema={currentRemoteSchema}
        permissionEdit={permissionEdit}
        isEditing={isEditing}
        schema={schema}
        bulkSelect={bulkSelect}
        readOnlyMode={readOnlyMode}
        permSetRoleName={permSetRoleName}
        permSetBulkSelect={permSetBulkSelect}
        setSchemaDefinition={setSchemaDefinition}
        permOpenEdit={permOpenEdit}
        permCloseEdit={permCloseEdit}
      />
      {!!bulkSelect.length && (
        <BulkSelect
          bulkSelect={bulkSelect}
          permRemoveMultipleRoles={permRemoveMultipleRoles}
        />
      )}
      <div className="mb-2">
        {!readOnlyMode && (
          <PermissionEditor
            key={permissionEdit.isNewRole ? 'NEW' : permissionEdit.role}
            permissionEdit={permissionEdit}
            isFetching={isFetching}
            isEditing={isEditing}
            schemaDefinition={schemaDefinition}
            remoteSchemaFields={remoteSchemaFields}
            permCloseEdit={permCloseEdit}
            saveRemoteSchemaPermission={handleSaveRemoteSchemaPermission}
            removeRemoteSchemaPermission={handlerRemoveRemoteSchemaPermission}
            setSchemaDefinition={setSchemaDefinition}
            introspectionSchema={schema}
          />
        )}
      </div>
    </div>
  );
};

export default Permissions;
