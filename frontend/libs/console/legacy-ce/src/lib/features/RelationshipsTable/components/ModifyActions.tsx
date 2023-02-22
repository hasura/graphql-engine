import React from 'react';
import { Button } from '../../../new-components/Button';
import { FaEdit, FaTrash } from 'react-icons/fa';
import { RelationshipType } from '../types';

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
  <div className="flex items-center justify-end whitespace-nowrap text-right gap-0.5">
    <Button onClick={() => onEdit(relationship)} icon={<FaEdit />}>
      Edit
    </Button>
    <Button onClick={() => onDelete(relationship)} icon={<FaTrash />}>
      Remove
    </Button>
  </div>
);

export default ModifyActions;
