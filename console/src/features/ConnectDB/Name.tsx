import { InputField } from '@/new-components/Form';
import React from 'react';

export const Name = ({ name }: { name: string }) => {
  return (
    <InputField
      type="text"
      placeholder="Enter a display name"
      name={name}
      label="Display Name"
      className="py-4"
    />
  );
};
