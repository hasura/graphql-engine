import { QueryCollection } from '@/metadata/types';
import React from 'react';
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
              onError: () => {
                // TODO: show global error notification
              },
              onSuccess: () => {
                // TODO: show global success notification
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
