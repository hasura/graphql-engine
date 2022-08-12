import React, { useCallback } from 'react';
import Button from '../../../Common/Button';
import { isJsonString, getConfirmation } from '../../../Common/utils/jsUtils';
import { FilterState } from './utils';
import { showErrorNotification } from '../../Common/Notification';
import { permChangePermissions, permChangeTypes } from './Actions';
import styles from '../../../Common/Permissions/PermissionStyles.scss';
import { Dispatch } from '../../../../types';

interface PermButtonSectionProps {
  readOnlyMode: string;
  query: string;
  localFilterString: FilterState;
  dispatch: Dispatch;
  permissionsState: FilterState;
  permsChanged: string;
  currQueryPermissions: string;
}

const PermButtonSection: React.FC<PermButtonSectionProps> = ({
  readOnlyMode,
  query,
  localFilterString,
  dispatch,
  permissionsState,
  permsChanged,
  currQueryPermissions,
}) => {
  const dispatchSavePermissions = useCallback(() => {
    const isInvalid = Object.values(localFilterString).some(val => {
      if (val && !isJsonString(val)) {
        return true;
      }
      return false;
    });

    if (isInvalid) {
      dispatch(
        showErrorNotification(
          'Saving permissions failed',
          'Row permission is not a valid JSON'
        )
      );
      return;
    }

    dispatch(permChangePermissions(permChangeTypes.save));
  }, [readOnlyMode, query, localFilterString, dispatch]);

  const dispatchRemoveAccess = useCallback(() => {
    const confirmMessage =
      'This will permanently delete the currently set permissions';
    const isOk = getConfirmation(confirmMessage);
    if (isOk) {
      dispatch(permChangePermissions(permChangeTypes.delete));
    }
  }, [dispatch]);

  if (readOnlyMode) {
    return null;
  }

  return (
    <div className={`${styles.add_mar_top} ${styles.add_pad_left}`}>
      <Button
        className={styles.add_mar_right}
        color="yellow"
        size="sm"
        onClick={dispatchSavePermissions}
        disabled={
          permissionsState.applySamePermissions.length !== 0 || !permsChanged
        }
        title={!permsChanged ? 'No changes made' : ''}
        data-test="Save-Permissions-button"
      >
        Save Permissions
      </Button>
      <Button
        className={styles.add_mar_right}
        color="red"
        size="sm"
        onClick={dispatchRemoveAccess}
        disabled={!currQueryPermissions}
        title={!currQueryPermissions ? 'No permissions set' : ''}
        data-test="Delete-Permissions-button"
      >
        Delete Permissions
      </Button>
    </div>
  );
};

export default PermButtonSection;
