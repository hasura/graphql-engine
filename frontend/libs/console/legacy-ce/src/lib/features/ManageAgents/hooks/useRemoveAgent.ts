import { useMetadataMigration } from '@hasura/metadata/api';
import { hasuraToast } from '@hasura/shared/ui';
import { useQueryClient } from '@tanstack/react-query';

export const useRemoveAgent = () => {
  const queryClient = useQueryClient();
  const { mutate, ...mutation } = useMetadataMigration({
    onSuccess: () => {
      hasuraToast({
        title: 'Success',
        type: 'success',
        message: 'Data connector agent removed successfully!',
      });
      queryClient.refetchQueries({ queryKey: ['agent_list'], exact: true });
    },
    onError: (err) => {
      hasuraToast({
        title: 'Error',
        type: 'error',
        message: err.toString(),
      });
    },
  });

  const removeAgent = ({
    name,
    onSuccess,
    onError,
  }: {
    name: string;
    onSuccess?: () => void;
    onError?: (err: any) => void;
  }) => {
    mutate(
      {
        query: {
          type: 'dc_delete_agent',
          args: {
            name,
          },
        },
      },
      {
        onSuccess: () => {
          if (onSuccess) onSuccess();
        },
        onError: (err) => {
          if (onError) onError(err);
        },
      },
    );
  };

  return {
    removeAgent,
    ...mutation,
  };
};
