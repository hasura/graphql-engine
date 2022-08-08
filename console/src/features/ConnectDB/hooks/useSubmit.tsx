import { useMetadataMigration } from '@/features/MetadataAPI';
import { APIError } from '@/hooks/error';
import { useFireNotification } from '@/new-components/Notifications';

export const useSubmit = () => {
  const { fireNotification } = useFireNotification();

  const { mutate, ...rest } = useMetadataMigration({
    onError: (error: APIError) => {
      fireNotification({
        type: 'error',
        title: 'Error',
        message: error?.message ?? 'Unable to connect to database',
      });
    },
    onSuccess: () => {
      fireNotification({
        type: 'success',
        title: 'Success',
        message: 'Successfully created database connection',
      });
    },
  });

  const submit = (values: { [key: string]: unknown }) => {
    mutate({
      query: {
        // TODO this needs to be dynamic based on the driver
        // unsure what this will look like with GDCs currently
        type: 'pg_add_source',
        args: values,
      },
    });
  };

  return { submit, ...rest };
};
