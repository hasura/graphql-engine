import React, { useEffect, useState } from 'react';
import { GraphQLSchema } from 'graphql';
import { generateSDL, getArgTreeFromPermissionSDL } from './utils';
import Button from '../../../Common/Button/Button';
import styles from '../../../Common/Permissions/PermissionStyles.scss';
import {
  RemoteSchemaFields,
  FieldType,
  ArgTreeType,
  PermissionEdit,
} from './types';
import { PermissionEditorContext } from './context';
import Tree from './Tree';
import { isEmpty } from '../../../Common/utils/jsUtils';

type PermissionEditorProps = {
  permissionEdit: PermissionEdit;
  isEditing: boolean;
  isFetching: boolean;
  schemaDefinition: string;
  remoteSchemaFields: RemoteSchemaFields[];
  introspectionSchema: GraphQLSchema;
  setSchemaDefinition: (data: string) => void;
  permCloseEdit: () => void;
  saveRemoteSchemaPermission: (
    successCb?: () => void,
    errorCb?: () => void
  ) => void;
  removeRemoteSchemaPermission: (
    successCb?: () => void,
    errorCb?: () => void
  ) => void;
};

const PermissionEditor: React.FC<PermissionEditorProps> = props => {
  const {
    permissionEdit,
    isEditing,
    isFetching,
    schemaDefinition,
    permCloseEdit,
    saveRemoteSchemaPermission,
    removeRemoteSchemaPermission,
    setSchemaDefinition,
    remoteSchemaFields,
    introspectionSchema,
  } = props;

  const [state, setState] = useState<RemoteSchemaFields[] | FieldType[]>(
    remoteSchemaFields
  );
  const [argTree, setArgTree] = useState<ArgTreeType>({}); // all @presets as an object tree
  const [resultString, setResultString] = useState(''); // Generated SDL

  const { isNewRole, isNewPerm } = permissionEdit;

  useEffect(() => {
    if (!state) return;
    setResultString(generateSDL(state, argTree));
  }, [state, argTree]);

  useEffect(() => {
    setState(remoteSchemaFields);
    setResultString(schemaDefinition);
  }, [remoteSchemaFields]);

  useEffect(() => {
    if (!isEmpty(schemaDefinition)) {
      try {
        const newArgTree = getArgTreeFromPermissionSDL(
          schemaDefinition,
          introspectionSchema
        );
        setArgTree(newArgTree);
      } catch (e) {
        console.error(e);
      }
    }
  }, [schemaDefinition]);

  if (!isEditing) return null;

  const buttonStyle = styles.add_mar_right;

  const closeEditor = () => {
    permCloseEdit();
  };

  const save = () => {
    saveRemoteSchemaPermission(closeEditor);
  };

  const saveFunc = () => {
    setSchemaDefinition(resultString);
    save();
  };

  const removeFunc = () => {
    removeRemoteSchemaPermission(closeEditor);
  };
  const scrollToElement = (path: string) => {
    let id = `type ${path}`;
    let el = document.getElementById(id);

    if (!el) {
      // input types
      id = `input ${path}`;
      el = document.getElementById(id);
    }

    if (el) {
      el.scrollIntoView({
        behavior: 'smooth',
        block: 'center',
        inline: 'nearest',
      });
      setTimeout(() => {
        // focusing element with css outline
        // there is no callback for scrollIntoView, this is a hack to make UX better,
        // simple implementation compared to adding another onscroll listener
        if (el) el.focus();
      }, 800);
    }
  };
  const isSaveDisabled = isEmpty(resultString) || isFetching;

  return (
    <div className={styles.activeEdit}>
      <div className={styles.tree}>
        <PermissionEditorContext.Provider
          value={{ argTree, setArgTree, scrollToElement }}
        >
          <Tree
            key={permissionEdit.isNewRole ? 'NEW' : permissionEdit.role}
            list={state as FieldType[]}
            setState={setState}
            permissionEdit={permissionEdit}
          />
          {/* below helps to debug the SDL */}
          {/* <code style={{ whiteSpace: 'pre-wrap' }}>{resultString}</code> */}
        </PermissionEditorContext.Provider>
      </div>
      <Button
        onClick={saveFunc}
        color="yellow"
        className={buttonStyle}
        disabled={isSaveDisabled}
        data-test="save-remote-schema-permissions"
      >
        Save Permissions
      </Button>
      {!(isNewRole || isNewPerm) && (
        <Button
          onClick={removeFunc}
          color="red"
          className={buttonStyle}
          disabled={isFetching}
          data-test="delete-remote-schema-permissions"
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
