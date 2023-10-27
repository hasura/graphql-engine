import { Dialog } from '../../../../../new-components/Dialog';
import React from 'react';
import { Relationship } from '../../../types';
import { useCreateTableRelationships } from '../../../hooks/useCreateTableRelationships/useCreateTableRelationships';
import {
  BulkAtomicResponse,
  BulkKeepGoingResponse,
} from '../../../../hasura-metadata-types';

interface ConfirmDeleteRelationshipPopupProps {
  relationship: Relationship;
  onCancel: () => void;
  onError: (err: Error) => void;
  onSuccess: (data: BulkAtomicResponse | BulkKeepGoingResponse) => void;
}

export const ConfirmDeleteRelationshipPopup = (
  props: ConfirmDeleteRelationshipPopupProps
) => {
  const { relationship, onCancel, onSuccess, onError } = props;

  const { deleteRelationships, isLoading } = useCreateTableRelationships(
    relationship.fromSource,
    {
      onSuccess,
      onError,
    }
  );

  return (
    <Dialog
      hasBackdrop
      title="Confirm Action"
      description="Please confirm if you want to proceeed with the action"
      onClose={onCancel}
      size="sm"
      footer={
        <Dialog.Footer
          onSubmit={() => {
            deleteRelationships({
              data: [
                {
                  name: relationship.name,
                  source: relationship.fromSource,
                  table: relationship.fromTable,
                },
              ],
            });
          }}
          onClose={onCancel}
          callToDeny="Cancel"
          callToAction="Drop Relationship"
          isLoading={isLoading}
        />
      }
    >
      <div className="mx-4 mb-sm">
        This will remove{' '}
        <span className="bg-gray-200 px-2 rounded-sm text-red-600">
          {relationship.name}
        </span>{' '}
        from Hasura. Please confirm you would like to go ahead with it.
      </div>
    </Dialog>
  );
};
