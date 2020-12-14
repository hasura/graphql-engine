import React, { useEffect, useState } from 'react';
import Helmet from 'react-helmet';
import * as GQL from 'graphql';
import PermissionsTable from './PermissionsTable';
import PermissionEditor from './PermissionEditor';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import { useIntrospectionSchemaRemote } from '../graphqlUtils';
import globals from '../../../../Globals';
import Button from '../../../Common/Button/Button';
import styles from '../../../Common/Permissions/PermissionStyles.scss';
import { getTree, getType } from './utils';
import { DatasourceObject } from './types';

const BulkSelectSection = ({ bulkSelect, permRemoveMultipleRoles }: any) => {
  const getSelectedRoles = () => {
    return bulkSelect.map((r: any) => {
      return (
        <span key={r} className={styles.add_pad_right}>
          <b>{r}</b>{' '}
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

declare const window: any;

const Permissions = ({ allRoles, ...props }: any) => {
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

  const { loading, error, schema, introspect } = useIntrospectionSchemaRemote(
    currentRemoteSchema.name,
    {
      'x-hasura-admin-secret': globals.adminSecret,
    },
    dispatch
  );

  useEffect(() => {
    if (!schema) return;
    const isNewRole: boolean = permissionEdit.isNewRole;
    let permissionsSchema: any = null;

    if (!isNewRole && !!schemaDefinition) {
      try {
        const directive = `scalar PresetValue\n
           directive @preset(
               value: PresetValue
           ) on INPUT_FIELD_DEFINITION | ARGUMENT_DEFINITION\n
         `;
        const newDef = directive + schemaDefinition;
        permissionsSchema = GQL.buildSchema(newDef);
        // console.log('>><<', permissionsSchema, schemaDefinition);
      } catch (err) {
        console.log(err);
      }
    }

    const types = getType(schema, permissionsSchema);

    setDatasource([
      {
        name: 'query_root',
        children: getTree(schema, permissionsSchema, 'QUERY'),
      },
      {
        name: 'mutation_root',
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
        <BulkSelectSection
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
            readOnlyMode={readOnlyMode}
            schemaDefinition={schemaDefinition}
            permCloseEdit={permCloseEdit}
            saveRemoteSchemaPermission={saveRemoteSchemaPermission}
            removeRemoteSchemaPermission={removeRemoteSchemaPermission}
            setSchemaDefinition={setSchemaDefinition}
            datasource={datasource}
            schema={schema}
          />
        )}
      </div>
    </div>
  );
};

export default Permissions;
