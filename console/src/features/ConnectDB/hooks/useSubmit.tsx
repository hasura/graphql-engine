import {
  allowedMetadataTypes,
  useMetadataMigration,
} from '@/features/MetadataAPI';
import { APIError } from '@/hooks/error';
import { useFireNotification } from '@/new-components/Notifications';

// all the other drivers the way we display the name is the same as the prefix
const getPrefix = (driver: string) => (driver === 'postgres' ? 'pg' : driver);

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
    const prefix = getPrefix(values.driver as string);

    mutate({
      query: {
        type: `${prefix}_add_source` as allowedMetadataTypes,
        args: values,
      },
    });
  };

  return { submit, ...rest };
};
