import { Select } from '@/new-components/Form';
import React from 'react';

export const RelationshipType = () => {
  return (
    <Select
      name="relationship_type"
      label="Relationship Type"
      dataTest="local-db-to-db-select-rel-type"
      placeholder="Select a relationship type..."
      options={[
        {
          label: 'Object Relationship',
          value: 'object',
        },
        {
          label: 'Array Relationship',
          value: 'array',
        },
      ]}
    />
  );
};
