import React from 'react';
import styles from '../../../Common/Permissions/PermissionStyles.scss';
import { getTypeFields } from '../graphqlUtils';
import GraphQLType from './GraphQLType';
import {
  setPermissionTypes,
  closePermissionEdit,
  createRemoteSchemaPermission,
  deleteRemoteSchemaPermission,
} from './Actions';
import Button from '../../../Common/Button/Button';
import { getExpandedTypes } from './utils';

const PermissionsEditor = ({
  editState,
  objectTypes,
  nonObjectTypes,
  rootTypes,
  dispatch,
  isFetching,
}) => {
  if (!editState.isEditing) {
    return null;
  }

  // get action buttons
  const getActionButtons = () => {
    const onSave = () => {
      dispatch(createRemoteSchemaPermission());
    };

    const onRemove = () => {
      dispatch(deleteRemoteSchemaPermission());
    };

    const onCancel = () => {
      dispatch(closePermissionEdit());
    };

    const getRemoveButton = () => {
      if (editState.isNew) return;
      return (
        <Button
          onClick={onRemove}
          className={`${styles.add_mar_right}`}
          color="red"
          disabled={isFetching}
        >
          Remove
        </Button>
      );
    };

    return (
      <div className={`${styles.display_flex} ${styles.add_mar_bottom}`}>
        <Button
          onClick={onSave}
          className={`${styles.add_mar_right}`}
          color="yellow"
          disabled={isFetching}
        >
          Save
        </Button>
        {getRemoveButton()}
        <Button
          onClick={onCancel}
          className={`${styles.add_mar_right}`}
          color="white"
        >
          Cancel
        </Button>
      </div>
    );
  };

  // get the expanded types based on the selected role and query type
  const expandedTypes = getExpandedTypes(
    editState.allowedTypes,
    rootTypes,
    editState.editType
  );

  // get allowed types
  const allowedTypes = Object.keys(editState.allowedTypes).map(at => {
    // on field toggle
    const fieldToggleCallback = (fieldName, isChecked) => {
      const newAllowedTypes = JSON.parse(
        JSON.stringify(editState.allowedTypes)
      );
      newAllowedTypes[at][fieldName].isChecked = isChecked;
      if (!newAllowedTypes[at][fieldName].isScalar) {
        if (!newAllowedTypes[newAllowedTypes[at][fieldName].typeName]) {
          const childFields = getTypeFields(
            newAllowedTypes[at][fieldName].typeName,
            objectTypes,
            nonObjectTypes
          );
          newAllowedTypes[
            newAllowedTypes[at][fieldName].typeName
          ] = childFields;
        }
      }
      dispatch(setPermissionTypes(newAllowedTypes));
    };

    // is the current type a root type
    const isRootType = ['query', 'mutation', 'subscription'].some(
      rt => rootTypes[rt] === at
    );

    // on removing an allowed type
    const typeRemovalCallback = () => {
      const newAllowedTypes = JSON.parse(
        JSON.stringify(editState.allowedTypes)
      );
      delete newAllowedTypes[at];
      Object.keys(editState.allowedTypes).forEach(_at => {
        Object.keys(editState.allowedTypes[_at]).forEach(field => {
          if (editState.allowedTypes[_at][field].typeName === at) {
            newAllowedTypes[_at][field].isChecked = false;
          }
        });
      });
      dispatch(setPermissionTypes(newAllowedTypes));
    };

    return (
      <div className={styles.add_mar_bottom_mid} key={at} id={at}>
        <GraphQLType
          typeName={at}
          fields={editState.allowedTypes[at]}
          fieldToggleCallback={fieldToggleCallback}
          isRootType={isRootType}
          isTypeExpanded={!!expandedTypes[at]}
          typeRemovalCallback={typeRemovalCallback}
        />
      </div>
    );
  });

  const permSelector = (
    <div>
      <div className={`${styles.add_mar_bottom_small}`}>
        <b>Allowed Types:</b>
      </div>
      <div
        className={`${styles.remoteSchemaPermSelector} ${styles.add_padding}`}
        key={editState.editType}
      >
        {allowedTypes}
      </div>
    </div>
  );

  return (
    <div className={styles.activeEdit}>
      {permSelector}
      {getActionButtons()}
    </div>
  );
};

export default PermissionsEditor;
