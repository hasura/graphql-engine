import React, { useEffect, useState } from 'react';
import { generateSDL, getArgTreeFromPermissionSDL } from './utils';
import Button from '../../../Common/Button/Button';
import styles from '../../../Common/Permissions/PermissionStyles.scss';
import {
  DatasourceObject,
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
  datasource: any;
  setSchemaDefinition: (data: string) => void;
  permCloseEdit: () => void;
  saveRemoteSchemaPermission: (data: any) => void;
  removeRemoteSchemaPermission: (data: any) => void;
};

const PermissionEditor: React.FC<PermissionEditorProps> = ({ ...props }) => {
  const {
    permissionEdit,
    isEditing,
    isFetching,
    schemaDefinition,
    permCloseEdit,
    saveRemoteSchemaPermission,
    removeRemoteSchemaPermission,
    setSchemaDefinition,
    datasource,
  } = props;

  const [state, setState] = useState<DatasourceObject[] | FieldType[]>(
    datasource
  );
  const [argTree, setArgTree] = useState<ArgTreeType>({}); // all @presets as an object tree
  const [resultString, setResultString] = useState(''); // Generated SDL

  const { isNewRole, isNewPerm } = permissionEdit;

  useEffect(() => {
    // console.log('changed--->', state);
    if (!state) return;
    setResultString(
      generateSDL(state as DatasourceObject[], argTree as Record<string, any>)
    );
  }, [state, argTree]);

  useEffect(() => {
    setState(datasource);
    setResultString(schemaDefinition);
  }, [datasource]);

  useEffect(() => {
    if (!isEmpty(schemaDefinition)) {
      try {
        const newArgTree = getArgTreeFromPermissionSDL(schemaDefinition);
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
          <Tree list={state as FieldType[]} setState={setState} />
          <code style={{ whiteSpace: 'pre-wrap' }}>{resultString}</code>
        </PermissionEditorContext.Provider>
      </div>
      <Button
        onClick={saveFunc}
        color="yellow"
        className={buttonStyle}
        disabled={isSaveDisabled}
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
