import React from 'react';
import Helmet from 'react-helmet';
import PermissionsTable from './PermissionsTable';
import PermissionEditor from './PermissionEditor';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import Button from '../../../Common/Button/Button';
import styles from '../../../Common/Permissions/PermissionStyles.scss';

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

const Permissions = ({ allRoles, ...props }: any) => {
  const {
    currentRemoteSchema,
    permissionEdit,
    isEditing,
    isFetching,
    bulkSelect,
    schemaDefinition,
    readOnlyMode = false,
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

  React.useEffect(() => {
    fetchRoleList();
    return () => {
      setDefaults();
    };
  }, []);

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
      {bulkSelect.length && (
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
          />
        )}
      </div>
    </div>
  );
};

export default Permissions;
