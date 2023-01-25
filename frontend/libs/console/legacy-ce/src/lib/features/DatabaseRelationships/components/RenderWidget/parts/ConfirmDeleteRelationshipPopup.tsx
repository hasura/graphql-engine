import { Dialog } from '@/new-components/Dialog';
import React from 'react';
import { Relationship } from '../../../types';
import { useManageLocalRelationship } from '../../../hooks/useManageLocalRelationship';

interface ConfirmDeleteRelationshipPopupProps {
  relationship: Relationship;
  onCancel: () => void;
  onError: (err: Error) => void;
  onSuccess: () => void;
}

export const ConfirmDeleteRelationshipPopup = (
  props: ConfirmDeleteRelationshipPopupProps
) => {
  const { relationship, onCancel, onSuccess, onError } = props;

  const { deleteRelationship } = useManageLocalRelationship({
    dataSourceName: relationship.fromSource,
    onSuccess,
    onError,
  });

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
            // Right now there is only support for local gdc relationship. We will add others as we release more features on the server
            if (relationship.type === 'localRelationship') {
              deleteRelationship(relationship);
            }
          }}
          onClose={onCancel}
          callToDeny="Cancel"
          callToAction="Drop Relationship"
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
