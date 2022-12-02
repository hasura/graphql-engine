import { useMetadataMigration } from '@/features/MetadataAPI';
import { useFireNotification } from '@/new-components/Notifications';
import { useCallback } from 'react';
import { useQueryClient } from 'react-query';

export const useAddAgent = () => {
  const { fireNotification } = useFireNotification();
  const queryClient = useQueryClient();
  const mutation = useMetadataMigration({
    onSuccess: async () => {
      fireNotification({
        title: 'Success',
        type: 'success',
        message: 'Data connector agent added successfully!',
      });
      queryClient.refetchQueries(['agent_list'], { exact: true });
    },
    onError: err => {
      fireNotification({
        title: 'Error',
        type: 'error',
        message: JSON.stringify(err),
      });
    },
  });

  const addAgent = useCallback(
    ({
      name,
      url,
      onSuccess,
      onError,
    }: {
      name: string;
      url: string;
      onSuccess?: () => void;
      onError?: (err: any) => void;
    }) => {
      mutation.mutate(
        {
          query: {
            type: 'dc_add_agent',
            args: {
              name,
              url,
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
    addAgent,
    isLoading: mutation.isLoading,
    isSuccess: mutation.isLoading,
    isError: mutation.isError,
    error: mutation.error,
  };
};
