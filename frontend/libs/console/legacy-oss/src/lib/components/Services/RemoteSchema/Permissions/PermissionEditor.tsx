import React, { useEffect, useState } from 'react';
import { GraphQLSchema } from 'graphql';
import { Button } from '@/new-components/Button';
import { generateSDL, getArgTreeFromPermissionSDL } from './utils';
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
    <div className="bg-white p-sm border border-gray-300">
      <div>
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
      <div className="mr-sm">
        <Button
          onClick={saveFunc}
          mode="primary"
          disabled={isSaveDisabled}
          data-test="save-remote-schema-permissions"
        >
          Save Permissions
        </Button>
      </div>
      {!(isNewRole || isNewPerm) && (
        <div className="mr-sm">
          <Button
            onClick={removeFunc}
            mode="destructive"
            disabled={isFetching}
            data-test="delete-remote-schema-permissions"
          >
            Remove Permissions
          </Button>
        </div>
      )}
      <Button onClick={closeEditor}>Cancel</Button>
    </div>
  );
};

export default PermissionEditor;
