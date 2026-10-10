import React from 'react';
import { Flex, Heading, Skeleton, Strong } from '@radix-ui/themes';
import {
  Button,
  Switch,
  IndicatorCard,
  Text,
  Table,
  Input,
  PermissionsIcon,
} from '@hasura/shared/ui';
import { FaPlusCircle } from 'react-icons/fa';
import { useSetRoleToAllowListPermission } from '../../hooks/AllowListPermissions/useSetRoleToAllowListPermissions';
import { useEnabledRolesFromAllowListState } from '../../hooks/AllowListPermissions/useEnabledRolesFromAllowListState';
import { useAddToAllowList } from '../../hooks/useAddToAllowList';
import { hasuraToast } from '@hasura/shared/ui';
import { useMetadata } from '@hasura/metadata/api';
import { MetadataSelectors } from '@hasura/metadata/helpers';

export interface AllowListPermissionsTabProps {
  collectionName: string;
}

export const AllowListPermissions: React.FC<AllowListPermissionsTabProps> = ({
  collectionName,
}) => {
  const { allAvailableRoles, newRoles, setNewRoles, enabledRoles } =
    useEnabledRolesFromAllowListState(collectionName);

  const { data: isCollectionInAllowlist, isLoading } = useMetadata(
    MetadataSelectors.isCollectionInAllowlist(collectionName),
  );

  const { setRoleToAllowListPermission } =
    useSetRoleToAllowListPermission(collectionName);

  const { addToAllowList, isLoading: addToAllowListLoading } =
    useAddToAllowList();

  const [updatingRoles, setUpdatingRoles] = React.useState<string[]>([]);

  if (isLoading) {
    return null;
  }

  const handleToggle = (roleName: string) => {
    setUpdatingRoles([...updatingRoles, roleName]);
    let newEnabledRoles: string[] = [];
    // add roleName to enabledRoles, remove duplicates
    if (enabledRoles.includes(roleName)) {
      newEnabledRoles = Array.from(
        new Set(enabledRoles.filter((role) => role !== roleName)),
      );
    } else {
      newEnabledRoles = Array.from(new Set([...enabledRoles, roleName]));
    }

    setRoleToAllowListPermission(newEnabledRoles, {
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          message: 'Allow list permissions updated',
          title: 'Success',
        });
        setUpdatingRoles(updatingRoles.filter((role) => role !== roleName));
      },
      onError: (e) => {
        hasuraToast({
          type: 'error',
          message: `Error updating allow list permissions: ${e.message}`,
          title: 'Error',
        });
        setUpdatingRoles(updatingRoles.filter((role) => role !== roleName));
      },
    });
  };

  const handleNewRole = (value: string, index: number) => {
    const newAddedRoles = [...newRoles];
    newAddedRoles[index] = value;

    // if last item is not an empty string, add empty string as last index
    if (newAddedRoles[newAddedRoles.length - 1] !== '') {
      newAddedRoles.push('');
    }
    // drop last one
    if (newAddedRoles[newAddedRoles.length - 2] === '') {
      newAddedRoles.pop();
    }

    setNewRoles(newAddedRoles);
  };

  if (!isCollectionInAllowlist) {
    return (
      <Flex
        direction="column"
        align="center"
        justify="center"
        className="h-full"
        gap="4"
      >
        <Heading size="5" color="gray">
          This collection is not in the allowlist
        </Heading>
        <Text as="p">
          Please add this collection to the allowlist to manage permissions
        </Text>
        <Button
          leftIcon={FaPlusCircle}
          mode="primary"
          className="mt-4"
          onClick={() => {
            addToAllowList(collectionName, {
              onSuccess: () => {
                hasuraToast({
                  type: 'success',
                  title: 'Collection added to allowlist',
                  message: `Collection ${collectionName} has been added to the allowlist`,
                });
              },
              onError: (e) => {
                hasuraToast({
                  type: 'error',
                  title: 'Collection not added to allowlist',
                  message: `Collection ${collectionName} could not be added to the allowlist: ${e.message}`,
                });
              },
            });
          }}
          loading={addToAllowListLoading}
        >
          Add to allowlist
        </Button>
      </Flex>
    );
  }

  return (
    <>
      <Table.Root variant="surface" className="min-w-full divide-y">
        <Table.Header>
          <Table.Row>
            <Table.RowHeaderCell className="uppercase tracking-wider">
              Role
            </Table.RowHeaderCell>
            <Table.RowHeaderCell className="uppercase tracking-wider">
              Access
            </Table.RowHeaderCell>
          </Table.Row>
        </Table.Header>
        <Table.Body>
          <Table.Row>
            <Table.ColumnHeaderCell>admin</Table.ColumnHeaderCell>
            <Table.Cell className="cursor-not-allowed">
              <PermissionsIcon type="fullAccess" />
            </Table.Cell>
          </Table.Row>
          {allAvailableRoles.map((roleName) => (
            <Table.Row key={roleName}>
              <Table.ColumnHeaderCell>{roleName}</Table.ColumnHeaderCell>
              <Table.Cell>
                <Skeleton loading={updatingRoles.length > 0}>
                  <Switch
                    value={enabledRoles.includes(roleName)}
                    onChange={() => handleToggle(roleName)}
                    data-testid={roleName}
                  />
                </Skeleton>
              </Table.Cell>
            </Table.Row>
          ))}
          {newRoles.map((newRole, index) => (
            <Table.Row key={index}>
              <Table.ColumnHeaderCell>
                <Input
                  x-model="newRole"
                  type="text"
                  placeholder="Create New Role..."
                  value={newRole}
                  onChange={(e) => handleNewRole(e.target.value, index)}
                />
              </Table.ColumnHeaderCell>
              <Table.Cell>
                <Skeleton loading={updatingRoles.length > 0}>
                  <Switch
                    value={enabledRoles.includes(newRole)}
                    onChange={
                      newRole !== '' ? () => handleToggle(newRole) : () => {}
                    }
                    data-testid={newRole}
                  />
                </Skeleton>
              </Table.Cell>
            </Table.Row>
          ))}
        </Table.Body>
      </Table.Root>
      {enabledRoles.length === 0 && (
        <IndicatorCard className="mt-4">
          The collection is in <Strong>global mode</Strong>: all users are
          enabled. If you want to assign permissions in a more granular way,
          enable specific roles. If you want to disable all users, remove the
          collection from the allow list{' '}
        </IndicatorCard>
      )}
    </>
  );
};
