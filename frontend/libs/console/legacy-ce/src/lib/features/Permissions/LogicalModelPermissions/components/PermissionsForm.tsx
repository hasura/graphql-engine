import { ReactNode } from 'react';
import {
  Button,
  Collapsible,
  CollapsibleHeader,
  IconTooltip,
  Badge,
  CheckboxGroup,
  createTextOption,
  Text,
  Card,
  RadioGroup,
} from '@hasura/shared/ui';
import { usePermissionsFormContext } from '../hooks/usePermissionForm';
import { Permission, RowSelectPermissionsType } from './types';
import { Flex, Strong } from '@radix-ui/themes';
import { getEdForm } from '@hasura/shared/utils';

type PermissionsFormProps = {
  permission: Permission;
  onSave: () => void;
  onDelete: () => void;
  PermissionsInput: ReactNode;
  isCreating?: boolean;
  isRemoving?: boolean;
};

export const PermissionsForm = ({
  permission,
  onDelete,
  onSave,
  PermissionsInput,
  isCreating,
  isRemoving,
}: PermissionsFormProps) => {
  const {
    unsetActivePermission,
    rowSelectPermissions,
    setRowSelectPermissions,
    columns,
    toggleColumn,
    toggleAllColumns,
    columnPermissionsStatus,
  } = usePermissionsFormContext();
  return (
    <form
      onSubmit={(e) => {
        e.preventDefault();
        onSave();
      }}
    >
      <Card data-testid="permissions-form">
        <Flex align="center" gap="4" className="pb-4">
          <Button
            mode="default"
            size="1"
            type="button"
            onClick={() => {
              unsetActivePermission();
            }}
          >
            Close
          </Button>
          <Flex data-testid="form-title" align="center" gap="2">
            <Text weight="bold">Role:</Text>
            <Badge className="mx-2" data-testid="role-pill">
              {permission.roleName}
            </Badge>
            <Text weight="bold">Action:</Text>
            <Badge className="mx-2" data-testid="action-pill">
              {permission.action}
            </Badge>
          </Flex>
        </Flex>
        <div className="mb-4">
          <RadioGroup
            value={rowSelectPermissions}
            onChange={(value) =>
              setRowSelectPermissions(value as RowSelectPermissionsType)
            }
            options={[
              {
                label: <NoChecksLabel />,
                value: 'without_filter',
              },
              {
                label: <CustomLabel />,
                value: 'with_custom_filter',
              },
            ]}
          />
        </div>
        {rowSelectPermissions === 'with_custom_filter' && (
          <div>{PermissionsInput}</div>
        )}
        <Collapsible
          triggerChildren={
            <CollapsibleHeader
              title={`Column ${permission.action} permissions`}
              tooltip={`Choose columns allowed to be ${getEdForm(
                permission.action,
              )}`}
              status={columnPermissionsStatus(permission)}
              // disabledMessage="Set row permissions first"
            />
          }
          defaultOpen
        >
          <div>
            <Text as="p">
              Allow role <Strong>{permission.roleName}</Strong> to access{' '}
              <Strong>columns</Strong>:
            </Text>
            <div className="mt-2">
              <CheckboxGroup
                orientation="horizontal"
                options={columns?.map(createTextOption) ?? []}
                value={permission.columns}
                onChange={(values) => toggleColumn(permission, values)}
              />
              <div className="mt-4">
                <Button
                  mode="default"
                  type="button"
                  size="1"
                  onClick={() => toggleAllColumns(permission)}
                  data-testid="toggle-all-columns"
                >
                  Toggle All
                </Button>
              </div>
            </div>
          </div>
        </Collapsible>

        <Flex gap="2" className="pt-2 mt-4" id="form-buttons-container">
          <Button
            loading={isCreating}
            disabled={isRemoving || isCreating}
            type="submit"
            mode="primary"
            title={'Submit'}
            data-testid="save-permissions-button"
          >
            Save Permissions
          </Button>

          <Button
            loading={isRemoving}
            disabled={isRemoving || isCreating}
            type="button"
            mode="destructive"
            onClick={onDelete}
            data-testid="delete-permissions-button"
          >
            Delete Permissions
          </Button>
        </Flex>
      </Card>
    </form>
  );
};

const NoChecksLabel = () => (
  <Text data-test="without-checks">Without any checks&nbsp;</Text>
);

const CustomLabel = () => (
  <Flex data-test="custom-check" align="center" gap="2">
    <Text>With custom check:</Text>
    <IconTooltip message="Create custom check using permissions builder" />
  </Flex>
);
