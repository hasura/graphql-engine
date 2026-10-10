import { TableFunction, MetadataFunction } from '@hasura/shared/types';
import { areFunctionsEqual, MetadataSelectors } from '@hasura/metadata/helpers';
import { getUntrackFunctionQuery } from './useUntrackFunctions';
import { getTrackFunctionQuery } from './useTrackFunction';
import {
  MetadataMigrationOptions,
  useMetadataHelpers,
  useMetadataMigration,
} from '../metadata';
import { hasuraToast } from '@hasura/shared/ui';
import { useErrorNotification } from '../notification';

export type RetrackFunctionArgs = {
  function: TableFunction;
  configuration?: MetadataFunction['configuration'];
  source: string;
  comment?: string;
};

export const useRetrackFunction = () => {
  const { mutate, ...rest } = useMetadataMigration();
  const { fetchMetadata } = useMetadataHelpers();
  const showErrorNotification = useErrorNotification();

  const retrackFunction = async (
    args: RetrackFunctionArgs,
    options?: MetadataMigrationOptions,
  ) => {
    const meta = await fetchMetadata();
    const source = MetadataSelectors.findSource(args.source)(meta);
    if (!source) {
      throw new Error(`Source ${args.source} does not exist`);
    }

    const oldFunction = source.functions?.find((fn) =>
      areFunctionsEqual(fn.function, args.function),
    );
    return mutate(
      {
        query: {
          type: 'bulk',
          args: [
            getUntrackFunctionQuery(
              {
                function: args.function,
                source: source.name,
              },
              source.kind,
            ),
            getTrackFunctionQuery(
              {
                function: args.function,
                source: source.name,
                comment: args.comment,
                configuration: {
                  ...oldFunction?.configuration,
                  ...args.configuration,
                },
              },
              source.kind,
            ),
          ],
          resource_version: meta.resource_version,
        },
      },
      {
        ...options,
        onSuccess: (data, variables, onMutateResult, context) => {
          hasuraToast({
            title: 'Success!',
            message: 'Update function successfully',
            type: 'success',
          });

          options?.onSuccess?.(data, variables, onMutateResult, context);
        },
        onError: (error, variables, onMutateResult, context) => {
          showErrorNotification({
            title: 'Error',
            error,
          });

          options?.onError?.(error, variables, onMutateResult, context);
        },
      },
    );
  };

  return { retrackFunction, ...rest };
};
