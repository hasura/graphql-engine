import React from 'react';
import { Button } from '@hasura/shared/ui';
import { RelationshipType } from '../types';
import { Strong } from '@radix-ui/themes';

type NameColumnCellProps = {
  relationship: RelationshipType;
  onClick: (rel: RelationshipType) => void;
};

const NameColumnCell = ({ relationship, onClick }: NameColumnCellProps) => {
  return (
    <Button variant="ghost" onClick={() => onClick(relationship)}>
      <Strong>{relationship?.name}</Strong>
    </Button>
  );
};

export default NameColumnCell;
