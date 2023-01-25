import React from 'react';
import { FaEdit, FaPencilAlt, FaTrash } from 'react-icons/fa';
import { MODE, Relationship } from '../../../types';

export const RowActions = ({
  relationship,
  onActionClick,
}: {
  relationship: Relationship;
  onActionClick: (relationship: Relationship, mode: MODE) => void;
}) => {
  return (
    <div className="flex items-center justify-end whitespace-nowrap text-right">
      {relationship.type === 'localRelationship' && (
        <button
          onClick={() => onActionClick(relationship, MODE.RENAME)}
          className="flex px-2 py-0.5 items-center font-semibold rounded text-secondary mr-0.5 hover:bg-indigo-50 focus:bg-indigo-100"
        >
          <FaPencilAlt className="fill-current mr-1" />
          Rename
        </button>
      )}

      {['remoteSchemaRelationship', 'remoteDatabaseRelationship'].includes(
        relationship.type
      ) && (
        <button
          onClick={() => onActionClick(relationship, MODE.EDIT)}
          className="flex px-2 py-0.5 items-center font-semibold rounded text-secondary mr-0.5 hover:bg-indigo-50 focus:bg-indigo-100"
        >
          <FaEdit className="fill-current mr-1" />
          Edit
        </button>
      )}

      <button
        onClick={() => onActionClick(relationship, MODE.DELETE)}
        className="flex px-2 py-0.5 items-center font-semibold rounded text-red-700 hover:bg-red-50 focus:bg-red-100"
      >
        <FaTrash className="fill-current mr-1" />
        Remove
      </button>
    </div>
  );
};
