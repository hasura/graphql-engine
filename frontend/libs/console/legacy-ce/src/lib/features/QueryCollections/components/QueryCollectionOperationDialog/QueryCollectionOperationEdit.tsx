import React from 'react';
import { QueryCollection } from '../../../../metadata/types';
import { useFireNotification } from '../../../../new-components/Notifications';

import { useEditOperationInQueryCollection } from '../../hooks/useEditOperationInQueryCollection';
import { QueryCollectionOperationDialog } from './QueryCollectionOperationDialog';

interface QueryCollectionOperationEditProps {
  queryCollectionName: string;
  operation: QueryCollection;
  onClose: () => void;
}
export const QueryCollectionOperationEdit = (
  props: QueryCollectionOperationEditProps
) => {
  const { onClose, operation, queryCollectionName } = props;
  const { isLoading, editOperationInQueryCollection } =
    useEditOperationInQueryCollection();
  const { fireNotification } = useFireNotification();
  return (
    <QueryCollectionOperationDialog
      title="Edit Operation"
      callToAction="Edit operation"
      isLoading={isLoading}
      onSubmit={values => {
        if (values.option === 'write operation') {
          editOperationInQueryCollection(
            queryCollectionName,
            operation.name,
            {
              name: values.name,
              query: values.query,
            },
            {
              onError: e => {
                fireNotification({
                  type: 'error',
                  title: 'Error',
                  message: `Failed to edit operation in query collection: ${e.message}`,
                });
              },
              onSuccess: () => {
                fireNotification({
                  type: 'success',
                  title: 'Success',
                  message: `Successfully edited operation in query collection`,
                });
                onClose();
              },
            }
          );
        }
      }}
      onClose={onClose}
      defaultValues={{
        option: 'write operation',
        name: operation.name,
        query: operation.query,
      }}
    />
  );
};
