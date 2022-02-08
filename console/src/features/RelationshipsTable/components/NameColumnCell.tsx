import React from 'react';
import { RelationshipType } from '../types';

type NameColumnCellProps = {
  relationship: RelationshipType;
  onClick: (rel: RelationshipType) => void;
};

const NameColumnCell = ({ relationship, onClick }: NameColumnCellProps) => {
  return (
    <button
      onClick={() => onClick(relationship)}
      className="text-secondary cursor-pointer"
    >
      {relationship?.name}
    </button>
  );
};

export default NameColumnCell;
