import React from 'react';
import { Button, Card, Text } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

export type BulkSelectProps = {
  bulkSelect: string[];
  permRemoveMultipleRoles: () => void;
};

const BulkSelect: React.FC<BulkSelectProps> = ({
  bulkSelect,
  permRemoveMultipleRoles,
}) => {
  const getSelectedRoles = () => {
    return bulkSelect.map((role: string) => {
      return (
        <Text key={role} weight="medium">
          {role}
        </Text>
      );
    });
  };

  const handleBulkRemoveClick = () => {
    // Confirmation is handled by useDropRemoteSchemaPermissionMultipleRoles.
    permRemoveMultipleRoles();
  };

  return (
    <Card id="bulk-section" size="2">
      <Text weight="bold" size="3">
        Apply Bulk Actions
      </Text>
      <Flex align="center" gap="2" className="mt-4">
        <Text>Selected Roles</Text>
        {getSelectedRoles()}
      </Flex>
      <div className="mt-2 mb-2">
        <Button onClick={handleBulkRemoveClick} mode="destructive" size="sm">
          Remove All Permissions
        </Button>
      </div>
    </Card>
  );
};

export default BulkSelect;
