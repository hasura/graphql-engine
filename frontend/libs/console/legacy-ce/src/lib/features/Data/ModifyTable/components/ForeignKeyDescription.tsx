import React from 'react';
import {
  TableFkRelationships,
  generateForeignKeyLabel,
} from '../../../DataSource';

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
