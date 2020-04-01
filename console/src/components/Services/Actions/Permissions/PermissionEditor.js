import React from 'react';

import Button from '../../../Common/Button/Button';
import { saveActionPermission, removeActionPermission } from '../ServerIO';
import { permCloseEdit } from './reducer';
import { Text } from '../../../UIKit/atoms';
import styles from '../../../Common/Permissions/PermissionStyles.scss';

const PermissionEditor = ({
  permissionEdit,
  isEditing,
  isFetching,
  dispatch,
}) => {
  if (!isEditing) return null;

  const { newRole, role, isNewRole, isNewPerm } = permissionEdit;

  const permRole = newRole || role;

  let permText = (
    <Text>
      This action is allowed for role: <b>{permRole}</b>
      <br />
      Click "Remove" if you wish to disallow it.
    </Text>
  );
  if (isNewPerm) {
    permText = (
      <Text>
        Click save to allow this action for role: <b>{permRole}</b>
      </Text>
    );
  }

  const buttonStyle = styles.add_mar_right;

  const closeEditor = () => {
    dispatch(permCloseEdit());
  };

  const getSaveButton = () => {
    if (!isNewPerm && !isNewRole) return null;
    const saveFunc = () => {
      dispatch(saveActionPermission(closeEditor));
    };
    return (
      <Button
        onClick={saveFunc}
        color="yellow"
        className={buttonStyle}
        disabled={isFetching}
      >
        Save
      </Button>
    );
  };

  const getRemoveButton = () => {
    if (isNewRole || isNewPerm) return;
    const removeFunc = () => {
      dispatch(removeActionPermission(closeEditor));
    };
    return (
      <Button
        onClick={removeFunc}
        color="red"
        className={buttonStyle}
        disabled={isFetching}
      >
        Remove
      </Button>
    );
  };

  const getCancelButton = () => {
    return (
      <Button color="white" className={buttonStyle} onClick={closeEditor}>
        Cancel
      </Button>
    );
  };

  return (
    <div className={styles.activeEdit}>
      <div className={styles.add_mar_bottom}>{permText}</div>
      {getSaveButton()}
      {getRemoveButton()}
      {getCancelButton()}
    </div>
  );
};

export default PermissionEditor;
