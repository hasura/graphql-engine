import React from 'react';
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
  <div className="flex items-center justify-end whitespace-nowrap text-right">
    <button
      onClick={() => onEdit(relationship)}
      className="flex px-2 py-0.5 items-center font-semibold rounded text-secondary mr-0.5 hover:bg-indigo-50 focus:bg-indigo-100"
    >
      <FaEdit className="fill-current mr-1" />
      Edit
    </button>
    <button
      onClick={() => onDelete(relationship)}
      className="flex px-2 py-0.5 items-center font-semibold rounded text-red-700 hover:bg-red-50 focus:bg-red-100"
    >
      <FaTrash className="fill-current mr-1" />
      Remove
    </button>
  </div>
);

export default ModifyActions;
