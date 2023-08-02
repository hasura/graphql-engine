import React from 'react';
import { Button } from '../../../../new-components/Button';
import { saveActionPermission, removeActionPermission } from '../ServerIO';
import { permCloseEdit } from './reducer';

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
    <div>
      This action is allowed for role: <b>{permRole}</b>
      <br />
      Click "Remove" if you wish to disallow it.
    </div>
  );
  if (isNewPerm) {
    permText = (
      <div>
        Click save to allow this action for role: <b>{permRole}</b>
      </div>
    );
  }

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
        mode="primary"
        disabled={isFetching}
        data-test="save-permissions-for-action"
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
        mode="destructive"
        size="md"
        disabled={isFetching}
      >
        Remove
      </Button>
    );
  };

  const getCancelButton = () => {
    return (
      <div className="ml-sm">
        <Button mode="default" onClick={closeEditor}>
          Cancel
        </Button>
      </div>
    );
  };

  return (
    <div className="bg-white mb-15 border-gray-300 p-3 border">
      <div className="mb-5">{permText}</div>
      <div className="flex">
        {getSaveButton()}
        {getRemoveButton()}
        {getCancelButton()}
      </div>
    </div>
  );
};

export default PermissionEditor;
