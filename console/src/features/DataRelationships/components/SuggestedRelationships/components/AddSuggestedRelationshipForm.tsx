import React from 'react';
import { Button } from '@/new-components/Button';
import { Form } from '@/new-components/Form';

import { DataTarget } from '@/features/Datasources';
import { TableRelationship } from '@/features/MetadataAPI';
import { schema, useSubmit } from '../hooks';

export interface SuggestedRelationshipProps {
  target: DataTarget;
}

interface SuggestedRelationshipFormProps {
  target: DataTarget;
  relationship: Omit<TableRelationship, 'name' | 'comment'>;
  close: () => void;
}

const CloseIcon = () => (
  <svg
    className="fill-current cursor-pointer w-4 h-4 text-muted hover:text-gray-900"
    stroke="currentColor"
    fill="currentColor"
    strokeWidth="0"
    viewBox="0 0 512 512"
    height="1em"
    width="1em"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path d="M256 8C119 8 8 119 8 256s111 248 248 248 248-111 248-248S393 8 256 8zm121.6 313.1c4.7 4.7 4.7 12.3 0 17L338 377.6c-4.7 4.7-12.3 4.7-17 0L256 312l-65.1 65.6c-4.7 4.7-12.3 4.7-17 0L134.4 338c-4.7-4.7-4.7-12.3 0-17l65.6-65-65.6-65.1c-4.7-4.7-4.7-12.3 0-17l39.6-39.6c4.7-4.7 12.3-4.7 17 0l65 65.7 65.1-65.6c4.7-4.7 12.3-4.7 17 0l39.6 39.6c4.7 4.7 4.7 12.3 0 17L312 256l65.6 65.1z" />
  </svg>
);

export const SuggestedRelationshipForm = ({
  target,
  relationship,
  close,
}: SuggestedRelationshipFormProps) => {
  const { submit, isLoading } = useSubmit();

  const handleSubmit = async ({
    relationshipName,
  }: Record<string, unknown>) => {
    try {
      await submit({
        relationshipName: relationshipName as string,
        target,
        relationship,
      });
      close();
    } catch (error) {
      console.error('Error while adding the relationship', error);
    }
  };

  return (
    <Form
      onSubmit={handleSubmit}
      schema={schema}
      options={{
        defaultValues: {
          relationshipName: relationship.to.table,
        },
      }}
      className="p-0"
    >
      {options => (
        <div className="flex items-center space-x-1.5 bg-white">
          <label htmlFor="relationshipName" className="sr-only">
            Relationship Name
          </label>
          <input
            id="relationshipName"
            type="text"
            className="block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-0 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
            placeholder="Relationship Name..."
            {...options.register('relationshipName')}
          />
          <Button type="submit" mode="primary" isLoading={isLoading}>
            Add Relationship
          </Button>
          <button onClick={close} aria-label="close">
            <CloseIcon />
          </button>
        </div>
      )}
    </Form>
  );
};
