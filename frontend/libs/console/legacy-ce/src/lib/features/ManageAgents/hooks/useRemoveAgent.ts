import { useMetadataMigration } from '@/features/MetadataAPI';
import { useFireNotification } from '@/new-components/Notifications';
import { useCallback } from 'react';
import { useQueryClient } from 'react-query';

export const useRemoveAgent = () => {
  const { fireNotification } = useFireNotification();
  const queryClient = useQueryClient();
  const mutation = useMetadataMigration({
    onSuccess: () => {
      fireNotification({
        title: 'Success',
        type: 'success',
        message: 'Data connector agent removed successfully!',
      });
      queryClient.refetchQueries(['agent_list'], { exact: true });
    },
    onError: () => {},
  });

  const removeAgent = useCallback(
    ({
      name,
      onSuccess,
      onError,
    }: {
      name: string;
      onSuccess?: () => void;
      onError?: (err: any) => void;
    }) => {
      mutation.mutate(
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
          onError: err => {
            if (onError) onError(err);
          },
        }
      );
    },
    [mutation]
  );

  return {
    removeAgent,
    isLoading: mutation.isLoading,
    isSuccess: mutation.isLoading,
    isError: mutation.isError,
    error: mutation.error,
  };
};
