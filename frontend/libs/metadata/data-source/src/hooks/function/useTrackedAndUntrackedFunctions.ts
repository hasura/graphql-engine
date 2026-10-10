import { Options, useMetadata } from '@hasura/metadata/api';
import {
  MetadataSelectors,
  adaptFunction,
  areFunctionsEqual,
} from '@hasura/metadata/helpers';
import { useTrackableFunctions } from './useTrackableFunctions';
import { IntrospectedFunction } from '../../driver';
import { MetadataFunction } from '@hasura/shared/types';

type TrackedAndUntrackedFunctionsResult = {
  trackedFunctions: TrackedFunction[];
  untrackedFunctions: IntrospectedFunction[];
};

export type TrackedFunction = IntrospectedFunction &
  Omit<MetadataFunction, 'function'>;

export const useTrackedAndUntrackedFunctions = (
  {
    dataSourceName,
    schema,
  }: {
    dataSourceName: string;
    schema?: string;
  },
  options?: Options,
) => {
  const {
    data: meta,
    isFetching: metadataFetching,
    ...metadataProps
  } = useMetadata();
  const source = MetadataSelectors.findSource(dataSourceName)(meta);
  const allTrackedFunctions = source?.functions ?? [];
  const trackedMetadataFunctions = schema
    ? allTrackedFunctions.filter(
        (fn) => adaptFunction(fn.function)?.schema === schema,
      )
    : allTrackedFunctions;

  const {
    data,
    isFetching: functionLoading,
    ...query
  } = useTrackableFunctions<TrackedAndUntrackedFunctionsResult>(
    {
      source: {
        name: dataSourceName,
        kind: source?.kind,
      },
    },
    {
      select: (introspectedFunctions) => {
        const schemaFunctions = schema
          ? introspectedFunctions.filter(
              (fn) => adaptFunction(fn.function).schema === schema,
            )
          : introspectedFunctions;

        return schemaFunctions.reduce(
          (acc, fn) => {
            const trackedFunction = trackedMetadataFunctions.find((trackedFn) =>
              areFunctionsEqual(fn.function, trackedFn.function),
            );

            if (trackedFunction) {
              acc.trackedFunctions.push({
                ...fn,
                configuration: trackedFunction.configuration,
                permissions: trackedFunction.permissions,
              });
            } else {
              acc.untrackedFunctions.push(fn);
            }

            return acc;
          },
          {
            trackedFunctions: [],
            untrackedFunctions: [],
          } as TrackedAndUntrackedFunctionsResult,
        );
      },
      staleTime: options?.staleTime,
      enabled: Boolean(source) || options?.enabled !== false,
    },
  );

  return {
    ...metadataProps,
    ...query,
    ...data,
    meta,
    source,
    isFetching: metadataFetching || functionLoading,
  };
};
