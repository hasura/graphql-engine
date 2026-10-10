import { Dialog, DialogFooter } from '@hasura/shared/ui';
import { Relationship } from '../../../types';
import { useDropRelationship } from '@hasura/metadata/api';

interface ConfirmDeleteRelationshipPopupProps {
  relationship: Relationship;
  onCancel: () => void;
  onError: (err: Error) => void;
  onSuccess: (data: unknown) => void;
}

export const ConfirmDeleteRelationshipPopup = (
  props: ConfirmDeleteRelationshipPopupProps,
) => {
  const { relationship, onCancel, onSuccess, onError } = props;
  const { dropRelationship, isPending } = useDropRelationship();

  return (
    <Dialog
      title="Confirm Action"
      description="Please confirm if you want to proceed with the action"
      onClose={onCancel}
      size="sm"
      footer={
        <DialogFooter
          onSubmit={() => {
            if (!relationship.fromTable) {
              return;
            }
            dropRelationship(
              {
                relationship: relationship.name,
                source: relationship.fromSource,
                table: relationship.fromTable,
              },
              {
                onSuccess,
                onError,
              },
            );
          }}
          onClose={onCancel}
          callToDeny="Cancel"
          callToAction="Drop Relationship"
          isLoading={isPending}
        />
      }
    >
      <div className="mx-4 mb-2">
        This will remove{' '}
        <span className="bg-gray-200 px-2 rounded-sm text-red-600">
          {relationship.name}
        </span>{' '}
        from Hasura. Please confirm you would like to go ahead with it.
      </div>
    </Dialog>
  );
};
