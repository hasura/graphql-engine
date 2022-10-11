import React from 'react';
import z from 'zod';
import { Dialog } from '@/new-components/Dialog';
import { Form, InputField } from '@/new-components/Form';
import { useFireNotification } from '@/new-components/Notifications';
import { useCreateQueryCollection } from '../../../QueryCollections/hooks/useCreateQueryCollection';

interface QueryCollectionCreateDialogProps {
  onClose: () => void;
  onCreate: (name: string) => void;
}

const schema = z.object({
  name: z.string().min(1, 'Name is required'),
});
export const QueryCollectionCreateDialog: React.FC<QueryCollectionCreateDialogProps> =
  props => {
    const { onClose, onCreate } = props;
    const { createQueryCollection, isLoading } = useCreateQueryCollection();
    const { fireNotification } = useFireNotification();

    return (
      <Form schema={schema} onSubmit={() => {}}>
        {({ watch, setError, trigger }) => {
          const name = watch('name');
          return (
            <Dialog hasBackdrop title="Create Collection" onClose={onClose}>
              <>
                <div className="p-4">
                  <InputField
                    id="name"
                    name="name"
                    label="New Collection Name"
                    placeholder="New Collection Name..."
                  />
                </div>
                <Dialog.Footer
                  callToDeny="Cancel"
                  callToAction="Create Collection"
                  onClose={onClose}
                  onSubmit={async () => {
                    if (await trigger()) {
                      // TODO: remove as when proper form types will be available
                      createQueryCollection(name as string, {
                        addToAllowList: true,
                        onSuccess: () => {
                          onClose();
                          onCreate(name as string);
                          fireNotification({
                            type: 'success',
                            title: 'Collection created',
                            message: `Collection ${name} was created successfully`,
                          });
                        },
                        onError: error => {
                          setError('name', {
                            type: 'manual',
                            message: (error as Error).message,
                          });
                          fireNotification({
                            type: 'error',
                            title: 'Collection creation failed',
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
          );
        }}
      </Form>
    );
  };
