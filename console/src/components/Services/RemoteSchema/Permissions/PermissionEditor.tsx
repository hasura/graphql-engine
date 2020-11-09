import React from 'react';
import Button from '../../../Common/Button/Button';
import styles from '../../../Common/Permissions/PermissionStyles.scss';
import GraphQLEditor from '../../../Common/GraphQLEditor/GraphQLEditor';

const PermissionEditor = ({ ...props }: any) => {
  const {
    permissionEdit,
    isEditing,
    isFetching,
    schemaDefinition,
    readOnlyMode,
    permCloseEdit,
    saveRemoteSchemaPermission,
    removeRemoteSchemaPermission,
    setSchemaDefinition,
  } = props;

  if (!isEditing) return null;

  const { isNewRole, isNewPerm } = permissionEdit;

  const {
    value: schemaDefinitionSdl,
    error: schemaDefinitionError,
    timer: schemaParseTimer,
  } = schemaDefinition;

  // TODO : types
  const schemaDefinitionOnChange = (
    value: any,
    error: any,
    timer: any,
    ast: any
  ) => {
    setSchemaDefinition({ value, error, timer, ast });
  };

  const buttonStyle = styles.add_mar_right;

  const closeEditor = () => {
    permCloseEdit();
  };

  const saveFunc = () => {
    saveRemoteSchemaPermission(closeEditor);
  };

  const removeFunc = () => {
    removeRemoteSchemaPermission(closeEditor);
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
          height="400px"
        />
      </div>
      <Button
        onClick={saveFunc}
        color="yellow"
        className={buttonStyle}
        disabled={isFetching}
      >
        Save Permissions
      </Button>
      {!(isNewRole || isNewPerm) && (
        <Button
          onClick={removeFunc}
          color="red"
          className={buttonStyle}
          disabled={isFetching}
        >
          Remove Permissions
        </Button>
      )}
      <Button color="white" className={buttonStyle} onClick={closeEditor}>
        Cancel
      </Button>
    </div>
  );
};

export default PermissionEditor;
