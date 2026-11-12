import { hasuraToast, showErrorNotification } from '@hasura/shared/ui';
import { QueryCollectionOperationDialog } from './QueryCollectionOperationDialog';
import { useAddOperationsToQueryCollection } from '@hasura/metadata/api';

interface QueryCollectionOperationAddProps {
  queryCollectionName: string;
  onClose: () => void;
}
export const QueryCollectionOperationAdd = (
  props: QueryCollectionOperationAddProps,
) => {
  const { onClose, queryCollectionName } = props;
  const { addOperationToQueryCollection, isPending } =
    useAddOperationsToQueryCollection();
  return (
    <QueryCollectionOperationDialog
      title="Add Operation"
      callToAction="Add Operation"
      isLoading={isPending}
      onSubmit={(values) => {
        if (values.option === 'write operation') {
          addOperationToQueryCollection(
            queryCollectionName,
            [{ name: values.name, query: values.query }],
            {
              onError: (e) => {
                showErrorNotification({
                  title: 'Failed to add operation to query collection',
                  error: e,
                });
              },
              onSuccess: () => {
                hasuraToast({
                  type: 'success',
                  title: 'Success',
                  message: `Successfully added operation to query collection`,
                });
                onClose();
              },
            },
          );

          return;
        }

        addOperationToQueryCollection(queryCollectionName, values.gqlFile, {
          onError: (e) => {
            showErrorNotification({
              title: 'Failed to add operation to query collection',
              error: e,
            });
          },
          onSuccess: () => {
            hasuraToast({
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
