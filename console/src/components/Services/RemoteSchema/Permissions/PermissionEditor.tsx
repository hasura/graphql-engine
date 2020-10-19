import React from 'react';
import Button from '../../../Common/Button/Button';
import styles from '../../../Common/Permissions/PermissionStyles.scss';
import {
  saveRemoteSchemaPermission,
  removeRemoteSchemaPermission,
} from '../Actions';
import { permCloseEdit, setSchemaDefinition } from './reducer';
import GraphQLEditor from '../../../Common/GraphQLEditor/GraphQLEditor';

const PermissionEditor = ({
  permissionEdit,
  isEditing,
  isFetching,
  dispatch,
  schemaDefinition,
  readOnlyMode,
}) => {
  if (!isEditing) return null;

  const { isNewRole, isNewPerm } = permissionEdit;

  const schemaDefinitionOnChange = (value, error, timer, ast) => {
    dispatch(setSchemaDefinition(value, error, timer, ast));
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

  const {
    sdl: schemaDefinitionSdl,
    error: schemaDefinitionError,
    timer: schemaParseTimer,
  } = schemaDefinition;

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
        <GraphQLEditor
          value={schemaDefinitionSdl}
          onChange={schemaDefinitionOnChange}
          error={schemaDefinitionError}
          timer={schemaParseTimer}
          readOnlyMode={readOnlyMode}
          allowEmpty={false}
          height='400px'
        />
      </div>
      {getSaveButton()}
      {getRemoveButton()}
      {getCancelButton()}
    </div>
  );
};

export default PermissionEditor;
