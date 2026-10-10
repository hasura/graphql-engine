import {
  useMetadataMigration,
  MetadataMigrationOptions,
  useMetadataHelpers,
} from '@hasura/metadata/api';
import {
  QualifiedStoredProcedure,
  StoredProcedure,
} from '@hasura/shared/types';
import { hasuraToast } from '@hasura/shared/ui';

export type TrackStoredProcedure = {
  dataSourceName: string;
} & StoredProcedure;

export const useTrackStoredProcedure = (
  globalMutateOptions?: MetadataMigrationOptions,
) => {
  const { fetchSource } = useMetadataHelpers();
  const { mutate, ...rest } = useMetadataMigration({
    ...globalMutateOptions,
    onSuccess: (data, variable, onMutateResult, context) => {
      globalMutateOptions?.onSuccess?.(data, variable, onMutateResult, context);
    },
  });

  const checkDriver = async (
    dataSourceName: string,
    action: 'Tracking' | 'Untracking',
  ): Promise<number | null> => {
    const { source, resource_version } = await fetchSource(dataSourceName);

    const driver = source.kind;
    if (driver !== 'mssql') {
      hasuraToast({
        type: 'error',
        title: `${action} store procedure failed`,
        message: 'Only MSSQL server supports store procedure',
      });
      return null;
    }

    return resource_version;
  };

  const trackStoredProcedure = async ({
    data: { dataSourceName, ...otherArgs },
    ...options
  }: {
    data: TrackStoredProcedure;
  } & MetadataMigrationOptions) => {
    const resourceVersion = await checkDriver(dataSourceName, 'Tracking');
    if (!resourceVersion) {
      return null;
    }

    return mutate(
      {
        query: {
          resource_version: resourceVersion,
          type: 'mssql_track_stored_procedure',
          args: {
            source: dataSourceName,
            ...otherArgs,
          },
        },
      },
      options,
    );
  };

  const untrackStoredProcedure = async ({
    data: { dataSourceName, stored_procedure },
    ...options
  }: {
    data: {
      dataSourceName: string;
      stored_procedure: QualifiedStoredProcedure;
    };
  } & MetadataMigrationOptions) => {
    const resourceVersion = await checkDriver(dataSourceName, 'Tracking');
    if (!resourceVersion) {
      return null;
    }

    return mutate(
      {
        query: {
          resource_version: resourceVersion,
          type: 'mssql_untrack_stored_procedure',
          args: {
            source: dataSourceName,
            stored_procedure,
          },
        },
      },
      options,
    );
  };

  return { trackStoredProcedure, untrackStoredProcedure, ...rest };
};
