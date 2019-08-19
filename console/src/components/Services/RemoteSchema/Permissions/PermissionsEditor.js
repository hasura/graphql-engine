import React, { useEffect } from 'react';
import styles from '../RemoteSchema.scss';
import { getTypeFields } from '../graphqlUtils';
import GraphQLType from './GraphQLType';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import {
  setPermissionRole,
  setPermissionTypes,
  createRemoteSchemaPermission,
} from './Actions';

const PermissionsEditor = ({
  permission,
  objectTypes,
  nonObjectTypes,
  rootTypes,
  index,
  isLast,
  dispatch,
}) => {
  const hydrateAllowedTypes = () => {
    if (isLast) {
      const alwaysAllowedTypes = {};
      Object.keys(rootTypes).forEach(rt => {
        const rootTypeName = rootTypes[rt];
        if (rootTypeName) {
          alwaysAllowedTypes[rootTypeName] = getTypeFields(
            rootTypeName,
            objectTypes,
            nonObjectTypes
          );
        }
      });
      dispatch(setPermissionTypes(alwaysAllowedTypes, index));
    }
  };
  useEffect(hydrateAllowedTypes, []);

  const editorExpanded = () => {
    const onChangeRole = e => {
      dispatch(setPermissionRole(e.target.value, index));
    };
    const roleTextbox = (
      <div className={`${styles.add_mar_bottom}`}>
        <div className={`${styles.add_mar_bottom_small}`}>
          <b>Role:</b>
        </div>
        <div>
          <input
            type="text"
            className={`form-control ${styles.wd300Px}`}
            value={permission.role}
            onChange={onChangeRole}
          />
        </div>
      </div>
    );

    const allowedTypes = Object.keys(permission.allowedTypes).map(at => {
      const fieldToggleCallback = (fieldName, isChecked) => {
        const newAllowedTypes = JSON.parse(
          JSON.stringify(permission.allowedTypes)
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
        dispatch(setPermissionTypes(newAllowedTypes, index));
      };

      const isRootType = ['query', 'mutation', 'subscription'].some(
        rt => rootTypes[rt] === at
      );

      const typeRemovalCallback = () => {
        const newAllowedTypes = JSON.parse(
          JSON.stringify(permission.allowedTypes)
        );
        delete newAllowedTypes[at];
        Object.keys(permission.allowedTypes).forEach(_at => {
          Object.keys(permission.allowedTypes[_at]).forEach(field => {
            if (permission.allowedTypes[_at][field].typeName === at) {
              newAllowedTypes[_at][field].isChecked = false;
            }
          });
        });
        dispatch(setPermissionTypes(newAllowedTypes, index));
      };

      return (
        <div className={styles.add_mar_bottom_mid} key={at} id={at}>
          <GraphQLType
            typeName={at}
            fields={permission.allowedTypes[at]}
            fieldToggleCallback={fieldToggleCallback}
            isRootType={isRootType}
            isTypeExpanded={!isRootType}
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
        >
          {allowedTypes}
        </div>
      </div>
    );

    return (
      <div>
        {roleTextbox}
        {permSelector}
      </div>
    );
  };

  const collapsedLabel = () => {
    if (isLast) return;
    return <b>{permission.role}</b>;
  };

  const expandButtonText = isLast ? 'Create a new permission' : 'Edit';
  const collapseButtonText = isLast ? 'Cancel' : 'Close';

  let saveFunc;
  let removeFunc;
  if (permission.role) {
    saveFunc = () => {
      dispatch(createRemoteSchemaPermission(index));
    };
  }

  return (
    <div key={index}>
      <ExpandableEditor
        editorExpanded={editorExpanded}
        property={'remote-relationship-add'}
        service="table-relationship"
        saveFunc={saveFunc}
        expandButtonText={expandButtonText}
        collapseButtonText={collapseButtonText}
        collapsedLabel={collapsedLabel}
        removeFunc={removeFunc}
      />
    </div>
  );
};

export default PermissionsEditor;
