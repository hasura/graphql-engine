import { useCallback } from 'react';
import { isObject } from '@hasura/shared/utils';
import {
  useMetadataMigration,
  MetadataMigrationOptions,
  useMetadata,
  useInvalidateSuggestedRelationships,
  TMigrationSingleQuery,
} from '@hasura/metadata/api';
import {
  BulkAtomicResponse,
  BulkKeepGoingResponse,
} from '@hasura/shared/types';
import {
  LocalTableRelationshipDefinition,
  RemoteSchemaRelationshipDefinition,
  RemoteTableRelationshipDefinition,
  TableRelationshipBasicDetails,
} from './types';
import { createTableRelationshipRequestBody } from './utils';
import { useAllDriverCapabilities } from '../../introspection';

type AllowedRelationshipDefinitions =
  | Omit<LocalTableRelationshipDefinition, 'capabilities'>
  | Omit<RemoteTableRelationshipDefinition, 'capabilities'>
  | Omit<RemoteSchemaRelationshipDefinition, 'capabilities'>;

type CreateTableRelationshipProps = Omit<
  TableRelationshipBasicDetails,
  'driver'
> & {
  definition: AllowedRelationshipDefinitions;
};

const defaultCapabilities = {
  isLocalTableRelationshipSupported: false,
  isRemoteTableRelationshipSupported: false,
  isRemoteSchemaRelationshipSupported: true,
};

const isRemoteRelPresentInPayload = (
  data: TMigrationSingleQuery[],
): boolean => {
  const remoteRel = data.find((rel) => {
    return rel.type.includes('_create_remote_relationship');
  });

  return !!remoteRel;
};

const getTargetName = (target: AllowedRelationshipDefinitions['target']) => {
  if ('toRemoteSchema' in target) return null;

  if ('toRemoteSource' in target) return target.toRemoteSource;

  return target.toSource;
};

export const useCreateTableRelationships = (
  dataSourceName: string,
  globalMutateOptions?: Omit<MetadataMigrationOptions, 'onSuccess'> & {
    onSuccess?: (
      data: BulkAtomicResponse | BulkKeepGoingResponse,
      variable?: any,
      ctx?: any,
    ) => void;
  },
) => {
  // get these capabilities

  const invalidateSuggestedRelationships = useInvalidateSuggestedRelationships({
    dataSourceName,
  });

  const { data: driverCapabilities = [] } = useAllDriverCapabilities({
    select: (data) => {
      const result = data.map((item) => {
        if (!item.capabilities)
          return {
            driver: item.driver,
            capabilities: {
              isLocalTableRelationshipSupported: false,
              isRemoteTableRelationshipSupported: false,
              isRemoteSchemaRelationshipSupported: false,
            },
          };
        return {
          driver: item.driver,
          capabilities: {
            isLocalTableRelationshipSupported: isObject(
              item.capabilities.relationships,
            ),
            isRemoteTableRelationshipSupported: isObject(
              item.capabilities.queries?.foreach,
            ),
            isRemoteSchemaRelationshipSupported: true,
          },
        };
      });

      return result;
    },
  });

  const { data: { metadataSources = [], resource_version } = {} } = useMetadata(
    (m) => ({
      metadataSources: m.metadata.sources,
      resource_version: m.resource_version,
    }),
  );

  const getDriver = useCallback(
    (source: string) => metadataSources.find((s) => s.name === source)?.kind,
    [metadataSources],
  );

  const { mutate, ...rest } = useMetadataMigration<
    BulkAtomicResponse | BulkKeepGoingResponse
  >({
    ...globalMutateOptions,
    onSuccess: (data, variable, ctx) => {
      globalMutateOptions?.onSuccess?.(data, variable, ctx);
      invalidateSuggestedRelationships();
    },
  });

  const createTableRelationships = useCallback(
    async (
      data: CreateTableRelationshipProps[],
      options?: MetadataMigrationOptions,
    ) => {
      const payloads = data
        .map((item) => {
          return createTableRelationshipRequestBody({
            driver: getDriver(item.source.fromSource) ?? '',
            name: item.name,
            source: {
              fromSource: item.source.fromSource,
              fromTable: item.source.fromTable,
            },
            definition: {
              ...item.definition,
            },
            sourceCapabilities:
              driverCapabilities.find(
                (c) => c.driver === getDriver(item.source.fromSource),
              )?.capabilities ?? defaultCapabilities,
            targetCapabilities:
              driverCapabilities.find(
                (c) =>
                  c.driver ===
                  getDriver(getTargetName(item.definition.target) ?? ''),
              )?.capabilities ?? defaultCapabilities,
          });
        })
        .filter(Boolean) as TMigrationSingleQuery[];

      return mutate(
        {
          query: {
            type: isRemoteRelPresentInPayload(payloads)
              ? 'bulk_keep_going'
              : 'bulk_atomic',
            args: payloads,
            resource_version,
          },
        },
        options,
      );
    },
    [driverCapabilities, getDriver, mutate, resource_version],
  );

  return {
    createTableRelationships,
    ...rest,
  };
};
