import React, { useState } from 'react';
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

  // Re-derive the remote schema fields whenever the introspected schema, the
  // new-role flag, or the SDL changes. Done during render (React's "adjust
  // state on prop change" pattern) instead of in an effect so it does not
  // cascade an extra render. `syncedFrom` tracks the inputs the current
  // `remoteSchemaFields` were built from; starting at `null` makes this run on
  // the first render where the async `schema` is available (matching the
  // previous mount effect that no-op'd until `schema` arrived).
  const [syncedFrom, setSyncedFrom] = useState<{
    schema: GraphQLSchema;
    isNewRole: boolean;
    schemaDefinition: string;
  } | null>(null);
  if (
    schema &&
    (syncedFrom === null ||
      syncedFrom.schema !== schema ||
      syncedFrom.isNewRole !== permissionEdit.isNewRole ||
      syncedFrom.schemaDefinition !== schemaDefinition)
  ) {
    setSyncedFrom({
      schema,
      isNewRole: permissionEdit.isNewRole,
      schemaDefinition,
    });

    const isNewRole: boolean = permissionEdit.isNewRole;

    // when server throws error while saving new role, do not reset the
    // remoteSchemaFields — persist the user defined schema in the UI
    if (!(isNewRole && schemaDefinition)) {
      let permissionsSchema: GraphQLSchema | null = null;
      if (!isNewRole && !!schemaDefinition) {
        permissionsSchema = buildSchemaFromRoleDefn(schemaDefinition);
      }
      setRemoteSchemaFields(getRemoteSchemaFields(schema, permissionsSchema));
    }
  }

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
