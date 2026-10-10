import React from 'react';
import { Button } from '@hasura/shared/ui';
import { FaEdit, FaTrash } from 'react-icons/fa';
import { RelationshipType } from '../types';
import { Flex } from '@radix-ui/themes';

interface ModifyActionsColProps {
  relationship: RelationshipType;
  onEdit: (rel: RelationshipType) => void;
  onDelete: (rel: RelationshipType) => void;
}

const ModifyActions = ({
  relationship,
  onEdit,
  onDelete,
}: ModifyActionsColProps) => (
  <Flex align="center" justify="end" gap="2">
    <Button
      size="1"
      mode="default"
      onClick={() => onEdit(relationship)}
      leftIcon={FaEdit}
    >
      Edit
    </Button>
    <Button
      size="1"
      mode="destructive"
      onClick={() => onDelete(relationship)}
      leftIcon={FaTrash}
    >
      Remove
    </Button>
  </Flex>
);

export default ModifyActions;
