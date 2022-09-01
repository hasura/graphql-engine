import React from 'react';
import { FaEdit, FaTrash } from 'react-icons/fa';
import { Button } from '@/new-components/Button';

interface ModifyActionsColProps {
  onEdit: () => void;
  onDelete: () => void;
}

export const ModifyActions = ({ onEdit, onDelete }: ModifyActionsColProps) => (
  <div className="flex items-center justify-end whitespace-nowrap text-right gap-0.5">
    <Button onClick={onEdit} icon={<FaEdit />}>
      Edit
    </Button>
    <Button onClick={onDelete} icon={<FaTrash />}>
      Remove
    </Button>
  </div>
);
