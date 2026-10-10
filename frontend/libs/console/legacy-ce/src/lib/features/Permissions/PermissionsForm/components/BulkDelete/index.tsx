import React from 'react';
import { Button, Text, useDestructiveConfirm } from '@hasura/shared/ui';
import { Card, Flex, Heading, Strong } from '@radix-ui/themes';
import { Table } from '@hasura/shared/types';
import { useBulkDeletePermissions } from '@hasura/metadata/api';

export interface BulkDeleteProps {
  dataSourceName: string;
  roles: string[];
  table: Table;
  handleClose: () => void;
}

export const BulkDelete: React.FC<BulkDeleteProps> = ({
  dataSourceName,
  roles,
  table,
  handleClose,
}) => {
  const destructiveConfirm = useDestructiveConfirm();
  const { mutateAsync: bulkDeletePermissions, isPending } =
    useBulkDeletePermissions({
      dataSourceName,
      table,
    });

  const handleDelete = async () => {
    destructiveConfirm({
      resourceName: `${roles.join(', ')}`,
      resourceType: 'permissions of the following roles',
      destroyTerm: 'delete',
      onConfirm: async () => {
        return bulkDeletePermissions({ roles })
          .then(() => {
            handleClose();
            return true;
          })
          .catch(() => false);
      },
    });
  };

  return (
    <Card size="3">
      <Heading size="3">Apply Bulk Actions</Heading>
      <Flex gap="2" className="my-4">
        <Text>
          <Strong>Selected Roles:</Strong>
        </Text>{' '}
        {roles.map((role) => (
          <Text key={role}>{role}</Text>
        ))}
      </Flex>
      <Button
        mode="destructive"
        type="button"
        loading={isPending}
        onClick={handleDelete}
      >
        Remove All Permissions
      </Button>
    </Card>
  );
};
