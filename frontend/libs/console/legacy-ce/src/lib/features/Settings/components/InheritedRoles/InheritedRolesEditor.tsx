import React, { useState, useEffect } from 'react';
import { Card, Flex, Strong } from '@radix-ui/themes';
import {
  Button,
  CheckboxGroup,
  Input,
  Text,
  Separator,
} from '@hasura/shared/ui';
import { FaSearch } from 'react-icons/fa';
import { InheritedRole } from '@hasura/shared/types';

type Mode = 'create' | 'edit';

export type EditorProps = {
  allRoles: string[];
  //  Pass the Inherited Role object when editing an existing Role
  inheritedRole?: InheritedRole | null;
  //  Pass the the Role name while creating a new Object.
  inheritedRoleName?: string;
  onSave: (inheritedRole: InheritedRole) => Promise<void>;
  isCollapsed: boolean;
  cancelCb: () => void;
};

const InheritedRolesEditor: React.FC<EditorProps> = ({
  allRoles,
  onSave,
  cancelCb,
  ...props
}) => {
  const [inheritedRoleName, setInheritedRoleName] = useState(
    props.inheritedRoleName,
  );
  const [inheritedRole, setInheritedRole] = useState(props.inheritedRole);
  const [isCollapsed, setIsCollapsed] = useState(props.isCollapsed);
  const [isSaving, setIsSaving] = useState(false);

  const [mode, setMode] = useState<Mode>(() =>
    inheritedRole ? 'edit' : 'create',
  );

  const defaultValues =
    mode === 'create'
      ? []
      : allRoles.filter((role) => inheritedRole?.role_set.includes(role));

  const [options, setOptions] = useState(defaultValues);

  useEffect(() => {
    setInheritedRoleName(props.inheritedRoleName);
    setInheritedRole(props.inheritedRole);
    setIsCollapsed(props.isCollapsed);
    const updatedMode = props.inheritedRole ? 'edit' : 'create';
    setMode(updatedMode);
    setOptions(
      allRoles.filter((role) => inheritedRole?.role_set.includes(role)),
    );
  }, [
    props.inheritedRoleName,
    props.inheritedRole,
    props.isCollapsed,
    allRoles,
  ]);

  const [filterText, setFilterText] = useState('');

  const filterTextChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    e.persist();
    setFilterText(e.target.value);
  };

  const selectAll = () => {
    setOptions(allRoles);
  };

  const clearAll = () => {
    setOptions([]);
  };

  const checkboxValueChange = (values: string[]) => {
    setOptions(values);
  };

  const saveRole = () => {
    const response: InheritedRole = {
      role_name: '',
      role_set: [],
    };

    if (mode === 'create') {
      response.role_name = inheritedRoleName || '';
    } else {
      response.role_name = inheritedRole?.role_name || '';
    }

    response.role_set = options;

    setIsSaving(true);
    onSave(response).finally(() => {
      setIsSaving(false);
    });
  };

  return (
    <>
      {!isCollapsed && (
        <Card>
          <div>
            <Flex align="center" gap="2">
              <Button
                mode="default"
                size="sm"
                disabled={isSaving}
                onClick={() => {
                  cancelCb();
                }}
              >
                Cancel
              </Button>
              {mode === 'create' ? (
                <Text as="div">
                  <Strong>Create Role:</Strong> {inheritedRoleName}{' '}
                </Text>
              ) : (
                <Text as="div">
                  <Strong>Edit Role:</Strong> {inheritedRole?.role_name}
                </Text>
              )}
            </Flex>
            <Separator my="4" size="4" />
            <Flex direction="column" gap="4">
              <Input
                icon={FaSearch}
                onChange={filterTextChange}
                value={filterText}
                placeholder="Filter Roles..."
                disabled={isSaving}
              />
              <div>
                <Button
                  mode="default"
                  size="2"
                  onClick={selectAll}
                  disabled={isSaving}
                >
                  Select all
                </Button>{' '}
                <Button
                  mode="default"
                  size="2"
                  onClick={clearAll}
                  disabled={isSaving}
                >
                  Clear all
                </Button>
              </div>
              <div>
                {!options.length ? (
                  'No singular/Non-inherited Roles available'
                ) : (
                  <CheckboxGroup
                    disabled={isSaving}
                    onChange={checkboxValueChange}
                    options={allRoles.map((option) => ({
                      label: option,
                      value: option,
                    }))}
                    value={options}
                  />
                )}
              </div>
            </Flex>
            <Separator my="4" size="4" />
            <div>
              <Button
                mode="primary"
                onClick={saveRole}
                disabled={isSaving || !options.length}
              >
                Save Role
              </Button>
            </div>
          </div>
        </Card>
      )}
    </>
  );
};

export default InheritedRolesEditor;
