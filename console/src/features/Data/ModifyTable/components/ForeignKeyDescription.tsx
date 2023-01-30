import React from 'react';
import {
  TableFkRelationships,
  generateForeignKeyLabel,
} from '@/features/DataSource';

export function ForeignKeyDescription({
  foreignKey,
}: {
  foreignKey: TableFkRelationships;
}) {
  const label = generateForeignKeyLabel(foreignKey);

  return (
    <div key={label} className="mb-2">
      {label}
    </div>
  );
}
