import React from 'react';
import Button from '../../../Common/Button/Button';
import styles from '../../../Common/Permissions/PermissionStyles.scss';
import {
  saveRemoteSchemaPermission,
  removeRemoteSchemaPermission,
} from '../Actions';
import { permCloseEdit, setSchemaDefinition } from './reducer';
import AceEditor from '../../../Common/AceEditor/BaseEditor';

const PermissionEditor = ({
  permissionEdit,
  isEditing,
  isFetching,
  dispatch,
}) => {
  if (!isEditing) return null;

  const { isNewRole, isNewPerm } = permissionEdit;

  const schemaDefinitionOnChange = value => {
    dispatch(setSchemaDefinition(value));
  };

  const buttonStyle = styles.add_mar_right;

  const closeEditor = () => {
    dispatch(permCloseEdit());
  };

  const getSaveButton = () => {
    if (!isNewPerm && !isNewRole) return null;
    const saveFunc = () => {
      dispatch(saveRemoteSchemaPermission(closeEditor));
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
      dispatch(removeRemoteSchemaPermission(closeEditor));
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
      <div className={styles.add_mar_bottom}>
        <AceEditor
          name="sdl-editor"
          onChange={schemaDefinitionOnChange}
          mode="graphqlschema"
          width="600px"
        />
      </div>
      {getSaveButton()}
      {getRemoveButton()}
      {getCancelButton()}
    </div>
  );
};

export default PermissionEditor;
