import { useAuthFetchJson } from '@hasura/shared/hooks';
import {
  useQuery,
  useQueryClient,
  UseQueryOptions,
} from '@tanstack/react-query';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource } from '@hasura/shared/types';
import {
  getDatabaseMethods,
  IntrospectedFunction,
  NotImplementedError,
} from '../../driver';
import { HttpError } from '@hasura/shared/types';
import { defaultQueryOptions } from '@hasura/metadata/api';
import { isObject } from '@hasura/shared/utils';
import { getTrackableFunctionsQueryKey } from '../../types/queryKey';

type Options<T = IntrospectedFunction[]> = Omit<
  UseQueryOptions<IntrospectedFunction[], HttpError, T>,
  'queryKey'
>;

export const useInvalidateTrackableFunctions = () => {
  const queryClient = useQueryClient();
  const invalidateIntrospectedFunction = (dataSourceName: string) => {
    queryClient.invalidateQueries({
      queryKey: getTrackableFunctionsQueryKey(dataSourceName),
    });
  };

  return invalidateIntrospectedFunction;
};

export const useTrackableFunctions = <T = IntrospectedFunction[]>(
  {
    source,
  }: {
    source: Partial<QualifiedDataSource>;
  },
  options?: Options<T>,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery({
    queryKey: getTrackableFunctionsQueryKey(source.name ?? ''),
    queryFn: async () => {
      if (!source.kind || !source.name) {
        return [];
      }

      const database = getDatabaseMethods(source.kind);
      if (!database.introspection.getTrackableFunctions) {
        throw new NotImplementedError();
      }

      const functions: IntrospectedFunction[] = [];
      const trackableFunctions =
        await database.introspection.getTrackableFunctions({
          dataSourceName: source.name,
          endpoints,
          fetchJson,
        });

      if (Array.isArray(trackableFunctions)) {
        functions.push(...trackableFunctions);
      }

      const getTrackableObjectsFn = database.introspection?.getTrackableObjects;

      if (getTrackableObjectsFn) {
        const trackableObjects = await getTrackableObjectsFn({
          dataSourceName: source.name,
          endpoints,
          fetchJson,
        });

        if (
          isObject(trackableObjects) &&
          'functions' in trackableObjects &&
          Array.isArray(trackableObjects.functions)
        ) {
          functions.push(...trackableObjects.functions);
        }
      }

      return functions;
    },
    ...defaultQueryOptions,
    ...options,
    enabled:
      Boolean(source?.name && source?.kind) && options?.enabled !== false,
  });
};
