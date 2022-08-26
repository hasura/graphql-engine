import { push } from 'react-router-redux';
import {
  allowedMetadataTypes,
  useMetadataMigration,
} from '@/features/MetadataAPI';
import { APIError } from '@/hooks/error';
import { useFireNotification } from '@/new-components/Notifications';
import { SupportedDrivers, getDriverPrefix } from '@/features/DataSource';
import { useAppDispatch } from '@/store';
import { exportMetadata } from '@/metadata/actions';
import { useAvailableDrivers } from './useAvailableDrivers';

// TODO this is temporary while we are still using the redux based manage page
const useRedirect = () => {
  const dispatch = useAppDispatch();
  const redirect = async () => {
    await dispatch(exportMetadata());
    dispatch(push('/data/manage'));
  };

  return redirect;
};

export const getAddSourceQueryType = (
  driver: SupportedDrivers
): allowedMetadataTypes => {
  const prefix = getDriverPrefix(driver);
  return `${prefix}_add_source`;
};

export const useSubmit = () => {
  const drivers = useAvailableDrivers();
  const { fireNotification } = useFireNotification();
  const redirect = useRedirect();

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

  // the values have to be unknown as zod generates the schema on the fly
  // based on the values returned from the API
  const submit = (values: { [key: string]: unknown }) => {
    if (!drivers.data)
      throw new Error('Unable to get valid drivers from metadata');

    if (typeof values.driver !== 'string')
      throw new Error('Invalid drivers format');

    if (!drivers.data?.map(driver => driver.name).includes(values.driver))
      throw new Error(`Unmanaged ${values.driver} driver`);

    mutate(
      {
        query: {
          type: getAddSourceQueryType(values.driver as SupportedDrivers),
          args: values,
        },
      },
      {
        onSuccess: () => {
          redirect();
        },
      }
    );
  };

  return { submit, ...rest };
};
