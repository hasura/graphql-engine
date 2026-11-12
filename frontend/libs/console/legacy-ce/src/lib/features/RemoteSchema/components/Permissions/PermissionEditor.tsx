import React, { useMemo, useState } from 'react';
import { GraphQLSchema } from 'graphql';
import { generateSDL, getArgTreeFromPermissionSDL } from './utils';
import {
  RemoteSchemaFields,
  FieldType,
  ArgTreeType,
  CustomFieldType,
} from './types';
import { PermissionEditorContext } from './context';
import Tree from './Tree';
import { isEmpty } from '@hasura/shared/utils';
import { Button, Card, Input, Text } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';
import { FaSearch } from 'react-icons/fa';

import { PermissionEdit } from '../../hooks/useRemoteSchemaRelationshipForm';

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
    schemaDefinition: string,
    successCb?: () => void,
    errorCb?: () => void,
  ) => void;
  removeRemoteSchemaPermission: (
    successCb?: () => void,
    errorCb?: () => void,
  ) => void;
};

const countVisible = (list: { name: string }[]) =>
  list.filter((field: any) => {
    return !['scalar', 'enum'].some((type) => {
      return field?.name?.toLowerCase().includes(type);
    });
  }).length;

const PermissionEditor: React.FC<PermissionEditorProps> = (props) => {
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
    remoteSchemaFields,
  );
  // all @presets as an object tree. Seeded from the permission SDL on mount via
  // lazy init (equivalent to the previous mount-time effect) and re-seeded on
  // later `schemaDefinition` changes through the adjust-during-render block
  // below.
  const [argTree, setArgTree] = useState<ArgTreeType>(() => {
    if (!isEmpty(schemaDefinition)) {
      try {
        return getArgTreeFromPermissionSDL(
          schemaDefinition,
          introspectionSchema,
        );
      } catch (e) {
        console.error(e);
      }
    }
    return {};
  });
  const [search, setSearch] = useState('');
  const { isNewRole, isNewPerm } = permissionEdit;

  // Reset the editable field tree whenever the `remoteSchemaFields` prop
  // changes. Done during render (React's "adjust state on prop change" pattern)
  // instead of in an effect so it does not cascade an extra render.
  const [prevRemoteSchemaFields, setPrevRemoteSchemaFields] =
    useState(remoteSchemaFields);
  if (remoteSchemaFields !== prevRemoteSchemaFields) {
    setPrevRemoteSchemaFields(remoteSchemaFields);
    setState(remoteSchemaFields);
  }

  // Re-seed the preset tree whenever the permission SDL changes.
  const [prevSchemaDefinition, setPrevSchemaDefinition] =
    useState(schemaDefinition);
  if (schemaDefinition !== prevSchemaDefinition) {
    setPrevSchemaDefinition(schemaDefinition);
    if (!isEmpty(schemaDefinition)) {
      try {
        setArgTree(
          getArgTreeFromPermissionSDL(schemaDefinition, introspectionSchema),
        );
      } catch (e) {
        console.error(e);
      }
    }
  }

  // The generated SDL is fully derived from the field tree and the preset tree,
  // so compute it during render instead of syncing it into state via an effect.
  const resultString = useMemo(
    () => (state ? generateSDL(state, argTree) : ''),
    [state, argTree],
  );

  if (!isEditing) return null;

  const buttonStyle = 'mr-2';

  const closeEditor = () => {
    permCloseEdit();
  };

  const saveFunc = () => {
    // kept so that the selection persists if saving fails
    setSchemaDefinition(resultString);
    // passed as is: the state set above is only read on the next render
    saveRemoteSchemaPermission(resultString, closeEditor);
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
          ['scalar', 'enum'].some((type) => {
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
      .map((field: RemoteSchemaFields | FieldType) => field.name),
  );

  return (
    <Card className="mt-4">
      <div>
        <PermissionEditorContext.Provider
          value={{ argTree, setArgTree, scrollToElement }}
        >
          <div className="relative">
            <Input
              className="mb-4"
              icon={FaSearch}
              value={search}
              placeholder="Search types..."
              clearable
              onChange={(ev) => {
                setSearch(ev.target.value);
              }}
            />
            {search && (
              <div className="mt-2">
                <Text color="gray">
                  {filteredTypes.size} out of {countVisible(state)} types found
                </Text>
              </div>
            )}
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
      <Flex gap="2" className="mt-4">
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
        <Button mode="default" className={buttonStyle} onClick={closeEditor}>
          Cancel
        </Button>
      </Flex>
    </Card>
  );
};

export default PermissionEditor;
