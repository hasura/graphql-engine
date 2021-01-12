import React, { useEffect, useState } from 'react';
import Helmet from 'react-helmet';
import { buildSchema, GraphQLSchema } from 'graphql';
import PermissionsTable from './PermissionsTable';
import PermissionEditor from './PermissionEditor';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import { useIntrospectionSchemaRemote } from '../graphqlUtils';
import globals from '../../../../Globals';
import Button from '../../../Common/Button/Button';
import styles from '../../../Common/Permissions/PermissionStyles.scss';
import { getTree, getType, addPresetDefinition } from './utils';
import { DatasourceObject, PermissionsProps, BulkSelectProps } from './types';

const BulkSelect: React.FC<BulkSelectProps> = ({
  bulkSelect,
  permRemoveMultipleRoles,
}) => {
  const getSelectedRoles = () => {
    return bulkSelect.map((role: string) => {
      return (
        <span key={role} className={styles.add_pad_right}>
          <b>{role}</b>{' '}
        </span>
      );
    });
  };

  const handleBulkRemoveClick = () => {
    const confirmMessage =
      'This will remove all currently set permissions for the selected role(s)';
    const isOk = getConfirmation(confirmMessage);
    if (isOk) {
      permRemoveMultipleRoles();
    }
  };

  return (
    <div id="bulk-section" className={styles.activeEdit}>
      <div className={styles.editPermsHeading}>Apply Bulk Actions</div>
      <div>
        <span className={styles.add_pad_right}>Selected Roles</span>
        {getSelectedRoles()}
      </div>
      <div className={`${styles.add_mar_top} ${styles.add_mar_bottom_mid}`}>
        <Button onClick={handleBulkRemoveClick} color="red" size="sm">
          Remove All Permissions
        </Button>
      </div>
    </div>
  );
};

const Permissions: React.FC<PermissionsProps> = ({ allRoles, ...props }) => {
  const {
    currentRemoteSchema,
    permissionEdit,
    isEditing,
    isFetching,
    bulkSelect,
    schemaDefinition,
    readOnlyMode = false,
    dispatch,
    fetchRoleList,
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

  const [datasource, setDatasource] = useState<DatasourceObject[]>([]);

  React.useEffect(() => {
    fetchRoleList();
    return () => {
      setDefaults();
    };
  }, [fetchRoleList, setDefaults]);

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
        // TODO test all posibilities that can reach this catch block
        console.log(err);
      }
    }
    console.log('>>> schema ', schema);
    const types = getType(schema, permissionsSchema);

    setDatasource([
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
  }, [schema, permissionEdit.isNewRole, schemaDefinition]);

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
            datasource={datasource}
            schema={schema}
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
