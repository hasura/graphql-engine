import { Button } from '@/new-components/Button';
import { z } from 'zod';
import { InputField, UpdatedForm } from '@/new-components/Form';
import React from 'react';
import { Relationship } from '../DatabaseRelationshipsTable/types';
import { useRenameRelationship } from './useRenameRelationship';

export const RenameRelationship = ({
  relationship,
  onError,
  onSuccess,
}: {
  relationship: Relationship;
  onSuccess?: () => void;
  onError?: (err: unknown) => void;
}) => {
  const { renameRelationship, isLoading } = useRenameRelationship();

  return (
    <div>
      <UpdatedForm
        schema={z.object({
          name: z.string(),
        })}
        onSubmit={data => {
          renameRelationship({
            values: {
              newName: data.name,
              relationshipName: relationship.name,
              fromTable: relationship.mapping.from.table,
              fromSource: relationship.mapping.from.source,
            },
            onSuccess,
            onError,
          });
        }}
        options={{
          defaultValues: {
            name: relationship.name,
          },
        }}
      >
        {() => (
          <div>
            <InputField name="name" type="text" label="Relationship Name" />
            <Button type="submit" mode="primary" isLoading={isLoading}>
              Rename
            </Button>
          </div>
        )}
      </UpdatedForm>
    </div>
  );
};
