import React from 'react';
import Helmet from 'react-helmet';
import PermissionsTable from './PermissionsTable';
import { fetchRoleList } from '../../Data/DataActions';
import styles from '../../../Common/Permissions/PermissionStyles.scss';
import PermissionEditor from './PermissionEditor';
import { setDefaults } from './reducer';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import { permRemoveMultipleRoles } from '../Actions';
import Button from '../../../Common/Button/Button';

const BulkSelectSection = ({ bulkSelect, dispatch }: any) => {
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
      dispatch(permRemoveMultipleRoles());
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

const Permissions = ({ allRoles, dispatch, ...props }: any) => {
  const {
    currentRemoteSchema,
    permissionEdit,
    isEditing,
    isFetching,
    bulkSelect,
    schemaDefinition,
    readOnlyMode = false,
  } = props;

  React.useEffect(() => {
    dispatch(fetchRoleList());
    return () => {
      dispatch(setDefaults());
    };
  }, []);

  return (
    <div>
      <Helmet
        title={`Permissions - ${currentRemoteSchema.name} - Remote Schemas | Hasura`}
      />
      <PermissionsTable allRoles={allRoles} dispatch={dispatch} {...props} />
      {bulkSelect.length && (
        <BulkSelectSection bulkSelect={bulkSelect} dispatch={dispatch} />
      )}
      <div className={`${styles.add_mar_bottom}`}>
        {!readOnlyMode && (
          <PermissionEditor
            permissionEdit={permissionEdit}
            dispatch={dispatch}
            isFetching={isFetching}
            isEditing={isEditing}
            readOnlyMode={readOnlyMode}
            schemaDefinition={schemaDefinition}
          />
        )}
      </div>
    </div>
  );
};

export default Permissions;
