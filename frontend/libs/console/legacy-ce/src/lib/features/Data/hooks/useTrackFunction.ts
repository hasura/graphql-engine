import { useCallback } from 'react';
import {
  MetadataMigrationOptions,
  useMetadataMigration,
} from '../../MetadataAPI/hooks/useMetadataMigration';
import { MetadataSelectors, useMetadata } from '../../hasura-metadata-api';
import {
  MetadataFunction,
  QualifiedFunction,
} from '../../hasura-metadata-types';
import { transformErrorResponse } from '../errorUtils';

export type MetadataFunctionPayload = {
  function: QualifiedFunction;
  configuration?: MetadataFunction['configuration'];
  source: string;
  comment?: string;
};

export const useTrackFunction = ({
  dataSourceName,
  ...globalMutateOptions
}: { dataSourceName: string } & MetadataMigrationOptions) => {
  const { mutate, ...rest } = useMetadataMigration({
    ...globalMutateOptions,
    onSuccess: (data, variables, ctx) => {
      globalMutateOptions?.onSuccess?.(data, variables, ctx);
    },
    errorTransform: transformErrorResponse,
  });

  const { data: { driver, resource_version } = {} } = useMetadata(m => ({
    driver: MetadataSelectors.findSource(dataSourceName)(m)?.kind,
    resource_version: m.resource_version,
  }));

  const trackFunction = useCallback(
    ({
      functionsToBeTracked,
      ...mutationOptions
    }: {
      functionsToBeTracked: {
        function: QualifiedFunction;
        configuration?: MetadataFunction['configuration'];
        comment?: string;
      }[];
    } & MetadataMigrationOptions) => {
      const payloads = functionsToBeTracked.map(fn => ({
        type: `${driver}_track_function`,
        args: { ...fn, source: dataSourceName },
      }));

      mutate(
        {
          query: {
            type: 'bulk',
            source: dataSourceName,
            resource_version,
            args: payloads,
          },
        },
        {
          ...mutationOptions,
        }
      );
    },
    [mutate, dataSourceName, resource_version, driver]
  );

  const untrackFunction = useCallback(
    ({
      functionsToBeUntracked,
      ...mutationOptions
    }: {
      functionsToBeUntracked: QualifiedFunction[];
    } & MetadataMigrationOptions) => {
      console.log(functionsToBeUntracked);

      const payloads = functionsToBeUntracked.map(fn => ({
        type: `${driver}_untrack_function`,
        args: {
          function: fn,
          source: dataSourceName,
        },
      }));

      mutate(
        {
          query: {
            type: 'bulk',
            source: dataSourceName,
            resource_version,
            args: payloads,
          },
        },
        {
          ...mutationOptions,
        }
      );
    },
    [mutate, dataSourceName, resource_version, driver]
  );

  return { trackFunction, untrackFunction, ...rest };
};
