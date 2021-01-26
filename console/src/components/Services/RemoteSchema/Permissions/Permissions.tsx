import React, { useEffect, useState } from 'react';
import Helmet from 'react-helmet';
import { buildSchema, GraphQLSchema } from 'graphql';
import PermissionsTable from './PermissionsTable';
import PermissionEditor from './PermissionEditor';
import { useIntrospectionSchemaRemote } from '../graphqlUtils';
import globals from '../../../../Globals';
import styles from '../../../Common/Permissions/PermissionStyles.scss';
import { getTree, getType, addPresetDefinition } from './utils';
import {
  RemoteSchemaFields,
  PermissionEdit,
  PermOpenEditType,
  PermissionsType,
} from './types';
import BulkSelect from './BulkSelect';
import { Dispatch } from '../../../../types';

export type PermissionsProps = {
  allRoles: string[];
  currentRemoteSchema: {
    name: string;
    permissions?: PermissionsType[];
  };
  bulkSelect: string[];
  readOnlyMode: boolean;
  permissionEdit: PermissionEdit;
  isEditing: boolean;
  isFetching: boolean;
  schemaDefinition: string;
  setSchemaDefinition: (data: string) => void;
  permOpenEdit: PermOpenEditType;
  permCloseEdit: () => void;
  permSetBulkSelect: (checked: boolean, role: string) => void;
  permSetRoleName: (name: string) => void;
  dispatch: Dispatch;
  fetchRoleList: () => void;
  setDefaults: () => void;
  saveRemoteSchemaPermission: (
    successCb?: () => void,
    errorCb?: () => void
  ) => void;
  removeRemoteSchemaPermission: (
    successCb?: () => void,
    errorCb?: () => void
  ) => void;
  permRemoveMultipleRoles: () => void;
};

const Permissions: React.FC<PermissionsProps> = props => {
  const {
    allRoles,
    currentRemoteSchema,
    permissionEdit,
    isEditing,
    isFetching,
    bulkSelect,
    schemaDefinition,
    readOnlyMode = false,
    dispatch,
    setDefaults,
    permCloseEdit,
    saveRemoteSchemaPermission,
    removeRemoteSchemaPermission,
    setSchemaDefinition,
    permRemoveMultipleRoles,
    permOpenEdit,
    permSetBulkSelect,
    permSetRoleName,
  } = props;

  const [remoteSchemaFields, setRemoteSchemaFields] = useState<RemoteSchemaFields[]>([]);

  React.useEffect(() => {
    return () => {
      setDefaults();
    };
  }, [setDefaults]);

  const { error, schema, introspect } = useIntrospectionSchemaRemote(
    currentRemoteSchema.name,
    {
      'x-hasura-admin-secret': globals.adminSecret,
    },
    dispatch
  );

  useEffect(() => {
    if (!schema) return;
    const isNewRole: boolean = permissionEdit.isNewRole;
    let permissionsSchema: GraphQLSchema | null = null;

    if (!isNewRole && !!schemaDefinition) {
      try {
        const newDef = addPresetDefinition(schemaDefinition);
        permissionsSchema = buildSchema(newDef);
      } catch (err) {
        console.log(err);
        return;
      }
    }
    const types = getType(schema, permissionsSchema);

    // when server throws error while saving new role, do not reset the remoteSchemaFields
    // persist the user defined schema in th UI
    if (isNewRole && schemaDefinition) return;

    setRemoteSchemaFields([
      {
        name: 'type query_root',
        typeName: 'query_root',
        children: getTree(schema, permissionsSchema, 'QUERY'),
      },
      {
        name: 'type mutation_root',
        typeName: 'mutation_root',
        children: getTree(schema, permissionsSchema, 'MUTATION'),
      },
      ...types,
    ]);
  }, [schema, permissionEdit?.isNewRole, schemaDefinition]);

  if (error || !schema) {
    return (
      <div>
        Error introspecting remote schema.{' '}
        <a onClick={introspect} className={styles.cursorPointer} role="button">
          {' '}
          Try again{' '}
        </a>
      </div>
    );
  }

  return (
    <div>
      <Helmet
        title={`Permissions - ${currentRemoteSchema.name} - Remote Schemas | Hasura`}
      />
      <PermissionsTable
        allRoles={allRoles}
        currentRemoteSchema={currentRemoteSchema}
        permissionEdit={permissionEdit}
        isEditing={isEditing}
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
      <div className={`${styles.add_mar_bottom}`}>
        {!readOnlyMode && (
          <PermissionEditor
            permissionEdit={permissionEdit}
            isFetching={isFetching}
            isEditing={isEditing}
            schemaDefinition={schemaDefinition}
            remoteSchemaFields={remoteSchemaFields}
            permCloseEdit={permCloseEdit}
            saveRemoteSchemaPermission={saveRemoteSchemaPermission}
            removeRemoteSchemaPermission={removeRemoteSchemaPermission}
            setSchemaDefinition={setSchemaDefinition}
          />
        )}
      </div>
    </div>
  );
};

export default Permissions;
