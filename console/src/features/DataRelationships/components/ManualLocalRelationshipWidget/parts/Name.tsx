import { InputField } from '@/new-components/Form';
import React from 'react';

export const Name = () => {
  return (
    <InputField
      name="name"
      label="Name"
      placeholder="Relationship name"
      dataTest="local-db-to-db-rel-name"
    />
  );
};
