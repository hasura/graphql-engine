import React from 'react';
import z from 'zod';
import { Dialog } from '../../../../new-components/Dialog';
import { InputField, useConsoleForm } from '../../../../new-components/Form';
import { useFireNotification } from '../../../../new-components/Notifications';
import { Analytics, REDACT_EVERYTHING } from '../../../Analytics';
import { useRenameQueryCollection } from '../../../QueryCollections/hooks/useRenameQueryCollection';

interface QueryCollectionCreateDialogProps {
  onClose: () => void;
  currentName: string;
  onRename: (currentName: string, newName: string) => void;
}

const schema = z.object({
  name: z.string().min(1, 'Name is required'),
});
export const QueryCollectionRenameDialog: React.FC<
  QueryCollectionCreateDialogProps
> = props => {
  const { onClose, currentName, onRename } = props;
  const { renameQueryCollection, isLoading } = useRenameQueryCollection();
  const { fireNotification } = useFireNotification();

  const {
    methods: { watch, setError, trigger },
    Form,
  } = useConsoleForm({
    schema,
  });
  const name = watch('name');

  return (
    <Form onSubmit={() => {}}>
      <Dialog hasBackdrop title="Rename Collection" onClose={onClose}>
        <>
          <Analytics name="QueryCollectionRenameDialog" {...REDACT_EVERYTHING}>
            <div className="p-4">
              <InputField
                id="name"
                name="name"
                label="New Collection Name"
                placeholder="New Collection Name..."
              />
            </div>
          </Analytics>
          <Dialog.Footer
            callToDeny="Cancel"
            callToAction="Rename Collection"
            onClose={onClose}
            onSubmit={async () => {
              if (await trigger()) {
                // TODO: remove as when proper form types will be available
                renameQueryCollection(currentName, name as string, {
                  onSuccess: () => {
                    onClose();
                    onRename(currentName, name as string);
                    fireNotification({
                      type: 'success',
                      title: 'Collection renamed',
                      message: `Collection ${currentName} was renamed to ${name}`,
                    });
                  },
                  onError: error => {
                    setError('name', {
                      type: 'manual',
                      message: (error as Error).message,
                    });
                    fireNotification({
                      type: 'error',
                      title: 'Error renaming collection',
                      message: (error as Error).message,
                    });
                  },
                });
              }
            }}
            isLoading={isLoading}
          />
        </>
      </Dialog>
    </Form>
  );
};
