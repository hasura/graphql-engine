import React, { useEffect, useState } from 'react';
import { GraphQLSchema } from 'graphql';
import { generateSDL, getArgTreeFromPermissionSDL } from './utils';
import {
  RemoteSchemaFields,
  FieldType,
  ArgTreeType,
  PermissionEdit,
  CustomFieldType,
} from './types';
import { PermissionEditorContext } from './context';
import Tree from './Tree';
import { isEmpty } from '../../../Common/utils/jsUtils';
import { InputField, useConsoleForm } from '../../../../new-components/Form';
import { z } from 'zod';
import { FaSearch, FaTimes } from 'react-icons/fa';
import { Button } from '../../../../new-components/Button';

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

const countVisible = (list: { name: string }[]) =>
  list.filter((field: any) => {
    return !['scalar', 'enum'].some(type => {
      return field?.name?.toLowerCase().includes(type);
    });
  }).length;

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

  const {
    methods: { watch, setValue },
    Form,
  } = useConsoleForm({
    schema: z.object({
      search: z.string(),
    }),
  });
  const search = watch('search');

  if (!isEditing) return null;

  const buttonStyle = 'mr-sm';

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

  const filteredTypes = new Set(
    state
      .filter((field: RemoteSchemaFields | FieldType) => {
        if (
          ['scalar', 'enum'].some(type => {
            return field?.name?.toLowerCase().includes(type);
          })
        ) {
          return false;
        }

        if (search === '') {
          return true;
        }

        return (
          field?.name?.toLowerCase().includes(search?.toLowerCase()) ||
          field.children?.some((child: CustomFieldType) => {
            return child?.name?.toLowerCase().includes(search?.toLowerCase());
          })
        );
      })
      .map((field: RemoteSchemaFields | FieldType) => field.name)
  );

  return (
    <div className="bg-white p-sm border border-gray-300">
      <div>
        <PermissionEditorContext.Provider
          value={{ argTree, setArgTree, scrollToElement }}
        >
          <div className="relative">
            <Form onSubmit={() => {}}>
              <InputField
                size="medium"
                icon={<FaSearch />}
                placeholder="Search types..."
                name="search"
                inputClassName="rounded-r-none"
                className="mb-4"
                noErrorPlaceholder
              />
              {search && (
                <FaTimes
                  className="cursor-pointer absolute left-[480px] top-[10px] text-gray-500"
                  onClick={() => setValue('search', '')}
                />
              )}
              {search && (
                <div className="text-gray-500 mb-2">
                  {filteredTypes.size} out of {countVisible(state)} types found
                </div>
              )}
            </Form>
          </div>
          <Tree
            key={permissionEdit.isNewRole ? 'NEW' : permissionEdit.role}
            list={state as FieldType[]}
            filteredTypes={filteredTypes}
            setState={setState}
            permissionEdit={permissionEdit}
          />
          {/* below helps to debug the SDL */}
          {/* <code style={{ whiteSpace: 'pre-wrap' }}>{resultString}</code> */}
        </PermissionEditorContext.Provider>
      </div>
      <div className="mt-4 flex gap-2">
        <Button
          onClick={saveFunc}
          mode="primary"
          disabled={isSaveDisabled}
          data-test="save-remote-schema-permissions"
        >
          Save Permissions
        </Button>
        {!(isNewRole || isNewPerm) && (
          <Button
            onClick={removeFunc}
            mode="destructive"
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
    </div>
  );
};

export default PermissionEditor;
