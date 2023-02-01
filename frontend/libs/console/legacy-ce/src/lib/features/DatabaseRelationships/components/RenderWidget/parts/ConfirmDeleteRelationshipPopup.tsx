import { Dialog } from '@/new-components/Dialog';
import React from 'react';
import { Relationship } from '../../../types';
import { useManageLocalRelationship } from '../../../hooks/useManageLocalRelationship';
import { useManageRemoteDatabaseRelationship } from '@/features/DatabaseRelationships/hooks/useManageRemoteDatabaseRelationship';
import { useManageRemoteSchemaRelationship } from '@/features/DatabaseRelationships/hooks/useManageRemoteSchemaRelationship';

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

  const {
    deleteRelationship: deleteLocalRelationship,
    isLoading: isDeleteLocalRelationshipLoading,
  } = useManageLocalRelationship({
    dataSourceName: relationship.fromSource,
    onSuccess,
    onError,
  });

  const {
    deleteRelationship: deleteRemoteDatabaseRelationship,
    isLoading: isDeleteRemoteDatabaseRelationshipLoading,
  } = useManageRemoteDatabaseRelationship({
    dataSourceName: relationship.fromSource,
    onSuccess,
    onError,
  });

  const {
    deleteRelationship: deleteRemoteSchemaRelationship,
    isLoading: isDeleteRemoteSchemaRelationshipLoading,
  } = useManageRemoteSchemaRelationship({
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
            if (relationship.type === 'localRelationship') {
              deleteLocalRelationship(relationship);
            } else if (relationship.type === 'remoteDatabaseRelationship') {
              deleteRemoteDatabaseRelationship(relationship);
            } else if (relationship.type === 'remoteSchemaRelationship')
              deleteRemoteSchemaRelationship(relationship);
          }}
          onClose={onCancel}
          callToDeny="Cancel"
          callToAction="Drop Relationship"
          isLoading={
            isDeleteLocalRelationshipLoading ||
            isDeleteRemoteDatabaseRelationshipLoading ||
            isDeleteRemoteSchemaRelationshipLoading
          }
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
