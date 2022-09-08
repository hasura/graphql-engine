import React from 'react';
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
  return (
    <QueryCollectionOperationDialog
      title="Add Operation"
      callToAction="Add operation"
      isLoading={isLoading}
      onSubmit={values => {
        if (values.option === 'write operation') {
          addOperationToQueryCollection(
            queryCollectionName,
            [{ name: values.name, query: values.query }],
            {
              onError: () => {
                // TODO: show global error notification
              },
              onSuccess: () => {
                // TODO: show global success notification
              },
            }
          );

          return;
        }

        addOperationToQueryCollection(queryCollectionName, values.gqlFile, {
          onError: () => {
            // TODO: show global error notification
          },
          onSuccess: () => {
            // TODO: show global success notification
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
