import React from 'react';
import { z } from 'zod';
import { Dialog } from '../../../../new-components/Dialog';
import { InputField, SimpleForm } from '../../../../new-components/Form';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { Relationship } from '../../types';
import { useCreateTableRelationships } from '../../hooks/useCreateTableRelationships/useCreateTableRelationships';
import {
  BulkAtomicResponse,
  BulkKeepGoingResponse,
} from '../../../hasura-metadata-types';

interface RenameRelationshipProps {
  relationship: Relationship;
  onCancel: () => void;
  onError?: (err: Error) => void;
  onSuccess?: (data: BulkAtomicResponse | BulkKeepGoingResponse) => void;
}

export const RenameRelationship = (props: RenameRelationshipProps) => {
  const { relationship, onCancel, onSuccess, onError } = props;
  const { renameRelationships } = useCreateTableRelationships(
    relationship.fromSource,
    { onSuccess, onError }
  );

  if (
    relationship.type === 'remoteDatabaseRelationship' ||
    relationship.type === 'remoteSchemaRelationship'
  )
    return (
      <IndicatorCard
        status="info"
        headline="Rename functionality is not available"
      >
        Please refer to the{' '}
        <a href="https://hasura.io/docs/latest/api-reference/metadata-api/remote-relationships/#introduction">
          docs
        </a>{' '}
        for more info about remote relationships
      </IndicatorCard>
    );

  return (
    <Dialog
      hasBackdrop
      title={`Rename: ${relationship.name}`}
      description="Rename your current relationship. "
      onClose={onCancel}
    >
      <SimpleForm
        options={{
          defaultValues: {
            updatedName: relationship.name,
          },
        }}
        schema={z.object({
          updatedName: z.string().min(1, 'Updated name cannot be empty!'),
        })}
        onSubmit={data => {
          renameRelationships({
            data: [
              {
                name: relationship.name,
                new_name: data.updatedName,
                source: relationship.fromSource,
                table: relationship.fromTable,
              },
            ],
          });
        }}
      >
        <>
          <div className="m-4">
            <InputField
              name="updatedName"
              label="New name"
              placeholder="Enter a new name"
              tooltip="New name of the relationship. Remember relationship names are unique."
            />
          </div>
          <Dialog.Footer
            callToDeny="Cancel"
            callToAction="Rename"
            onClose={onCancel}
            isLoading={false}
          />
        </>
      </SimpleForm>
    </Dialog>
  );
};
