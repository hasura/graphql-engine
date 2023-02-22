import React from 'react';
import { useFireNotification } from '../../../../new-components/Notifications';

import { useAddOperationsToQueryCollection } from '../../hooks';
import { QueryCollectionOperationDialog } from './QueryCollectionOperationDialog';

interface QueryCollectionOperationAddProps {
  queryCollectionName: string;
  onClose: () => void;
}
export const QueryCollectionOperationAdd = (
  props: QueryCollectionOperationAddProps
) => {
  const { onClose, queryCollectionName } = props;
  const { addOperationToQueryCollection, isLoading } =
    useAddOperationsToQueryCollection();
  const { fireNotification } = useFireNotification();
  return (
    <QueryCollectionOperationDialog
      title="Add Operation"
      callToAction="Add Operation"
      isLoading={isLoading}
      onSubmit={values => {
        if (values.option === 'write operation') {
          addOperationToQueryCollection(
            queryCollectionName,
            [{ name: values.name, query: values.query }],
            {
              onError: e => {
                fireNotification({
                  type: 'error',
                  title: 'Error',
                  message: `Failed to add operation to query collection: ${e.message}`,
                });
              },
              onSuccess: () => {
                fireNotification({
                  type: 'success',
                  title: 'Success',
                  message: `Successfully added operation to query collection`,
                });
                onClose();
              },
            }
          );

          return;
        }

        addOperationToQueryCollection(queryCollectionName, values.gqlFile, {
          onError: e => {
            fireNotification({
              type: 'error',
              title: 'Error',
              message: `Failed to add operation to query collection: ${e.message}`,
            });
          },
          onSuccess: () => {
            fireNotification({
              type: 'success',
              title: 'Success',
              message: `Successfully added operation to query collection`,
            });
            onClose();
          },
        });
      }}
      onClose={onClose}
      defaultValues={{
        option: 'write operation',
        name: '',
        query: '',
      }}
    />
  );
};
