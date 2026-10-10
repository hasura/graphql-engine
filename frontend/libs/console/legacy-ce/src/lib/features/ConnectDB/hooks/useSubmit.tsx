import { useQueryClient } from '@tanstack/react-query';
import { APIError } from '../../../hooks/error';
import { useMetadataMigration, useMetadata } from '@hasura/metadata/api';
import { SupportedDriver } from '@hasura/shared/types';
import { useAvailableDrivers } from '@hasura/metadata/data-source';
import { hasuraToast } from '@hasura/shared/ui';
import { useNavigate } from 'react-router';
import { getDriverPrefix } from '@hasura/metadata/helpers';

export const getAddSourceQueryType = (driver: SupportedDriver) => {
  const prefix = getDriverPrefix(driver);
  return `${prefix}_add_source` as const;
};

export const getEditSourceQueryType = (driver: SupportedDriver) => {
  const prefix = getDriverPrefix(driver);
  return `${prefix}_update_source` as const;
};

const useRedirect = () => {
  const navigate = useNavigate();
  const { refetch: refreshMetadata } = useMetadata(undefined, {
    enabled: false,
  });

  return async () => {
    await refreshMetadata();
    navigate({
      pathname: '/data/manage',
    });
  };
};

export const useSubmit = () => {
  const drivers = useAvailableDrivers();
  const redirect = useRedirect();
  const { mutate, ...rest } = useMetadataMigration({
    onError: (error: APIError) => {
      hasuraToast({
        type: 'error',
        title: 'Error',
        message: error?.message ?? 'Unable to connect to database',
      });
    },
    onSuccess: () => {
      hasuraToast({
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

    if (
      !drivers.data
        ?.map((driver) => driver.name)
        .includes(values.driver as 'mysql' | SupportedDriver)
    )
      throw new Error(`Unmanaged ${values.driver} driver`);

    mutate(
      {
        query: {
          type: getAddSourceQueryType(values.driver as SupportedDriver),
          args: values,
        },
      },
      {
        onSuccess: () => {
          redirect();
        },
      },
    );
  };

  return { submit, ...rest };
};

export const useEditDataSourceConnection = () => {
  const redirect = useRedirect();
  const queryClient = useQueryClient();

  const { mutate, ...rest } = useMetadataMigration({
    onError: (error: APIError) => {
      hasuraToast({
        type: 'error',
        title: 'Error',
        message: error?.message ?? 'Unable to connect to database',
      });
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['treeview'] });
      queryClient.invalidateQueries({ queryKey: ['edit-connection'] });

      hasuraToast({
        type: 'success',
        title: 'Success',
        message: 'Successfully created database connection',
      });
    },
  });

  // the values have to be unknown as zod generates the schema on the fly
  // based on the values returned from the API
  const submit = (values: { [key: string]: unknown }) => {
    mutate(
      {
        query: {
          type: getEditSourceQueryType(values.driver as SupportedDriver),
          args: values,
        },
      },
      {
        onSuccess: redirect,
      },
    );
  };

  return { submit, ...rest };
};
