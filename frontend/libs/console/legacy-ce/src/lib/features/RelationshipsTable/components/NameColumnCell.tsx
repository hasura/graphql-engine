import React from 'react';
import { Button } from '../../../new-components/Button';
import { RelationshipType } from '../types';

type NameColumnCellProps = {
  relationship: RelationshipType;
  onClick: (rel: RelationshipType) => void;
};

const NameColumnCell = ({ relationship, onClick }: NameColumnCellProps) => {
  return (
    <Button onClick={() => onClick(relationship)}>{relationship?.name}</Button>
  );
};

export default NameColumnCell;
