import React from 'react';
import z from 'zod';
import {
  Dialog,
  InputField,
  useConsoleForm,
  hasuraToast,
  DialogFooter,
} from '@hasura/shared/ui';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { useRenameQueryCollection } from '@hasura/metadata/api';

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
> = (props) => {
  const { onClose, currentName, onRename } = props;
  const { renameQueryCollection, isPending } = useRenameQueryCollection();
  const {
    methods: { watch, setError, trigger },
    Form,
  } = useConsoleForm({
    schema,
  });
  const name = watch('name');

  return (
    <Form onSubmit={() => {}}>
      <Dialog title="Rename Collection" onClose={onClose}>
        <>
          <Analytics name="QueryCollectionRenameDialog" {...REDACT_EVERYTHING}>
            <div className="p-4">
              <InputField
                id="name"
                name="name"
                label="New Collection Name"
                fieldProps={{
                  placeholder: 'New Collection Name...',
                }}
              />
            </div>
          </Analytics>
          <DialogFooter
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
                    hasuraToast({
                      type: 'success',
                      title: 'Collection renamed',
                      message: `Collection ${currentName} was renamed to ${name}`,
                    });
                  },
                  onError: (error) => {
                    setError('name', {
                      type: 'manual',
                      message: (error as Error).message,
                    });
                    hasuraToast({
                      type: 'error',
                      title: 'Error renaming collection',
                      message: (error as Error).message,
                    });
                  },
                });
              }
            }}
            isLoading={isPending}
          />
        </>
      </Dialog>
    </Form>
  );
};
