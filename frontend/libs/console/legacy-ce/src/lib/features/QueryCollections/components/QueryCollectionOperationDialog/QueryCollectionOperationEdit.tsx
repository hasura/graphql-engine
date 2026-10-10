import { hasuraToast } from '@hasura/shared/ui';
import { QueryCollectionQuery } from '@hasura/shared/types';
import { QueryCollectionOperationDialog } from './QueryCollectionOperationDialog';
import { useEditOperationInQueryCollection } from '@hasura/metadata/api';

interface QueryCollectionOperationEditProps {
  queryCollectionName: string;
  operation: QueryCollectionQuery;
  onClose: () => void;
}
export const QueryCollectionOperationEdit = (
  props: QueryCollectionOperationEditProps,
) => {
  const { onClose, operation, queryCollectionName } = props;
  const { isPending, editOperationInQueryCollection } =
    useEditOperationInQueryCollection();
  return (
    <QueryCollectionOperationDialog
      title="Edit Operation"
      callToAction="Edit operation"
      isLoading={isPending}
      onSubmit={(values) => {
        if (values.option === 'write operation') {
          editOperationInQueryCollection(
            queryCollectionName,
            operation.name,
            {
              name: values.name,
              query: values.query,
            },
            {
              onError: (e) => {
                hasuraToast({
                  type: 'error',
                  title: 'Error',
                  message: `Failed to edit operation in query collection: ${e.message}`,
                });
              },
              onSuccess: () => {
                hasuraToast({
                  type: 'success',
                  title: 'Success',
                  message: `Successfully edited operation in query collection`,
                });
                onClose();
              },
            },
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
