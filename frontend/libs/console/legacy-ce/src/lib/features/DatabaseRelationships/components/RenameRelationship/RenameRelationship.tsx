import { z } from 'zod';
import {
  Dialog,
  InputField,
  SimpleForm,
  IndicatorCard,
  DialogFooter,
} from '@hasura/shared/ui';
import { Relationship } from '../../types';
import { useRenameRelationship } from '@hasura/metadata/api';

interface RenameRelationshipProps {
  relationship: Relationship;
  onCancel: () => void;
  onError?: (err: Error) => void;
  onSuccess?: (data: unknown) => void;
}

export const RenameRelationship = (props: RenameRelationshipProps) => {
  const { relationship, onCancel, onSuccess, onError } = props;
  const { renameRelationship } = useRenameRelationship();

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
        onSubmit={(data) => {
          if (!relationship.fromTable) {
            return;
          }

          renameRelationship(
            {
              name: relationship.name,
              new_name: data.updatedName,
              source: relationship.fromSource,
              table: relationship.fromTable,
            },
            { onSuccess, onError },
          );
        }}
      >
        <>
          <div className="m-4">
            <InputField
              name="updatedName"
              label="New name"
              tooltip="New name of the relationship. Remember relationship names are unique."
              fieldProps={{
                placeholder: 'Enter a new name',
              }}
            />
          </div>
          <DialogFooter
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
