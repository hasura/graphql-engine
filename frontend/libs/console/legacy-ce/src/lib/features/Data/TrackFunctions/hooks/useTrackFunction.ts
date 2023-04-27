import { useCallback, useMemo } from 'react';
import { APIError } from '../../../../hooks/error';
import { hasuraToast } from '../../../../new-components/Toasts';
import {
  MetadataSelectors,
  useInvalidateMetadata,
  useMetadata,
} from '../../../hasura-metadata-api';
import {
  MetadataFunction,
  QualifiedFunction,
} from '../../../hasura-metadata-types';
import { useMetadataMigration } from '../../../MetadataAPI';

export type MetadataFunctionPayload = {
  function: QualifiedFunction;
  configuration?: MetadataFunction['configuration'];
  source: string;
  comment?: string;
};

export const useTrackFunction = ({
  dataSourceName,
  onSuccess,
  onError,
}: {
  dataSourceName: string;
  onSuccess?: () => void;
  onError?: (err: unknown) => void;
}) => {
  const { mutate, ...rest } = useMetadataMigration();
  const invalidateMetadata = useInvalidateMetadata();
  const { data: driver } = useMetadata(
    m => MetadataSelectors.findSource(dataSourceName)(m)?.kind
  );

  const mutationOptions = useMemo(
    () => ({
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          title: 'Tracked Successfully!',
        });
        onSuccess?.();
        invalidateMetadata();
      },
      onError: (err: APIError) => {
        console.log(err.message);
        hasuraToast({
          type: 'error',
          title: 'Failed to track!',
          message: err.message,
        });
        onError?.(err);
      },
    }),
    [invalidateMetadata, onError, onSuccess]
  );

  const trackFunction = useCallback(
    (values: MetadataFunctionPayload) => {
      mutate(
        {
          query: {
            type: `${driver}_track_function`,
            args: values,
          },
        },
        {
          ...mutationOptions,
        }
      );
    },
    [mutate, mutationOptions, driver]
  );

  return { trackFunction, ...rest };
};
