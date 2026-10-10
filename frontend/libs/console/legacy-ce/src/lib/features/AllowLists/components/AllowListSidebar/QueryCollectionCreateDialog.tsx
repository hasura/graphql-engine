import React from 'react';
import z from 'zod';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { useCreateQueryCollection } from '@hasura/metadata/api';
import {
  hasuraToast,
  Dialog,
  InputField,
  useConsoleForm,
  DialogFooter,
} from '@hasura/shared/ui';

interface QueryCollectionCreateDialogProps {
  onClose: () => void;
  onCreate: (name: string) => void;
}

const schema = z.object({
  name: z.string().min(1, 'Name is required'),
});
export const QueryCollectionCreateDialog: React.FC<
  QueryCollectionCreateDialogProps
> = (props) => {
  const { onClose, onCreate } = props;
  const { createQueryCollection, isPending } = useCreateQueryCollection();

  const {
    methods: { trigger, watch, setError },
    Form,
  } = useConsoleForm({
    schema,
  });
  const name = watch('name');

  return (
    <Form onSubmit={() => {}}>
      <Dialog title="Create Collection" onClose={onClose}>
        <>
          <Analytics name="AllowList" {...REDACT_EVERYTHING}>
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
            callToAction="Create Collection"
            onClose={onClose}
            onSubmit={async () => {
              if (await trigger()) {
                // TODO: remove as when proper form types will be available
                createQueryCollection(
                  { name, addToAllowList: true },
                  {
                    onSuccess: () => {
                      onClose();
                      onCreate(name as string);
                      hasuraToast({
                        type: 'success',
                        title: 'Collection created',
                        message: `Collection ${name} was created successfully`,
                      });
                    },
                    onError: (error) => {
                      setError('name', {
                        type: 'manual',
                        message: (error as Error).message,
                      });
                      hasuraToast({
                        type: 'error',
                        title: 'Collection creation failed',
                        message: (error as Error).message,
                      });
                    },
                  },
                );
              }
            }}
            isLoading={isPending}
          />
        </>
      </Dialog>
    </Form>
  );
};
