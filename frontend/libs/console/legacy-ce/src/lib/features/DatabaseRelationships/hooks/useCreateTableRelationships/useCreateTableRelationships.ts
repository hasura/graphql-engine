import { useCallback } from 'react';
import { isObject } from '../../../../components/Common/utils/jsUtils';
import { transformErrorResponse } from '../../../Data/errorUtils';
import { useAllDriverCapabilities } from '../../../Data/hooks/useAllDriverCapabilities';
import { Feature } from '../../../DataSource';
import { useMetadataMigration } from '../../../MetadataAPI';
import { MetadataMigrationOptions } from '../../../MetadataAPI/hooks/useMetadataMigration';
import { areTablesEqual, useMetadata } from '../../../hasura-metadata-api';
import {
  BulkAtomicResponse,
  BulkKeepGoingResponse,
  Table,
} from '../../../hasura-metadata-types';
import {
  DeleteRelationshipProps,
  LocalTableRelationshipDefinition,
  RemoteSchemaRelationshipDefinition,
  RemoteTableRelationshipDefinition,
  RenameRelationshipProps,
  TableRelationshipBasicDetails,
} from './types';
import {
  createTableRelationshipRequestBody,
  deleteTableRelationshipRequestBody,
  renameRelationshipRequestBody,
} from './utils';
import { useInvalidateSuggestedRelationships } from '../../../Data/TrackResources/TrackRelationships/hooks/useSuggestedRelationships';

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

type RenameTableRelationshipProps = Omit<RenameRelationshipProps, 'driver'>;

type DeleteTableRelationshipProps = Omit<
  DeleteRelationshipProps,
  'driver' | 'isRemote'
>;

const defaultCapabilities = {
  isLocalTableRelationshipSupported: false,
  isRemoteTableRelationshipSupported: false,
  isRemoteSchemaRelationshipSupported: true,
};

const isRemoteRelPresentInPayload = (
  data: ReturnType<typeof createTableRelationshipRequestBody>[]
) => {
  const remoteRel = data.find(rel => {
    if (rel === 'Not implemented') return false;

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
      ctx?: any
    ) => void;
  }
) => {
  // get these capabilities

  const { invalidateSuggestedRelationships } =
    useInvalidateSuggestedRelationships({
      dataSourceName,
    });

  const { data: driverCapabilties = [] } = useAllDriverCapabilities({
    select: data => {
      const result = data.map(item => {
        if (item.capabilities === Feature.NotImplemented)
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
              item.capabilities.relationships
            ),
            isRemoteTableRelationshipSupported: isObject(
              item.capabilities.queries?.foreach
            ),
            isRemoteSchemaRelationshipSupported: true,
          },
        };
      });

      return result;
    },
  });

  const { data: { metadataSources = [], resource_version } = {} } = useMetadata(
    m => ({
      metadataSources: m.metadata.sources,
      resource_version: m.resource_version,
    })
  );

  const getDriver = useCallback(
    (source: string) => metadataSources.find(s => s.name === source)?.kind,
    [metadataSources]
  );

  const isRemoteRelationship = useCallback(
    (source: string, table: Table, relName: string) => {
      return !!metadataSources
        .find(s => s.name === source)
        ?.tables.find(t => areTablesEqual(t.table, table))
        ?.remote_relationships?.find(r => r.name === relName);
    },
    [metadataSources]
  );

  const { mutate, ...rest } = useMetadataMigration<
    BulkAtomicResponse | BulkKeepGoingResponse
  >({
    ...globalMutateOptions,
    errorTransform: transformErrorResponse,
    onSuccess: (data, variable, ctx) => {
      globalMutateOptions?.onSuccess?.(data, variable, ctx);
      invalidateSuggestedRelationships();
    },
  });

  const createTableRelationships = useCallback(
    async ({
      data,
      ...options
    }: { data: CreateTableRelationshipProps[] } & MetadataMigrationOptions) => {
      const payloads = data.map(item => {
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
            driverCapabilties.find(
              c => c.driver.kind === getDriver(item.source.fromSource)
            )?.capabilities ?? defaultCapabilities,
          targetCapabilities:
            driverCapabilties.find(
              c =>
                c.driver.kind ===
                getDriver(getTargetName(item.definition.target) ?? '')
            )?.capabilities ?? defaultCapabilities,
        });
      });

      mutate(
        {
          query: {
            type: isRemoteRelPresentInPayload(payloads)
              ? 'bulk_keep_going'
              : 'bulk_atomic',
            args: payloads,
            resource_version,
          },
        },
        options
      );
    },
    [driverCapabilties, getDriver, mutate, resource_version]
  );

  const renameRelationships = useCallback(
    async ({
      data,
      ...options
    }: { data: RenameTableRelationshipProps[] } & MetadataMigrationOptions) => {
      const payloads = data.map(item => {
        return renameRelationshipRequestBody({
          driver: getDriver(item.source) ?? '',
          name: item.name,
          source: item.source,
          new_name: item.new_name,
          table: item.table,
        });
      });

      mutate(
        {
          query: {
            type: 'bulk_keep_going',
            args: payloads,
            resource_version,
          },
        },
        options
      );
    },
    [getDriver, mutate, resource_version]
  );

  const deleteRelationships = useCallback(
    async ({
      data,
      ...options
    }: { data: DeleteTableRelationshipProps[] } & MetadataMigrationOptions) => {
      const payloads = data.map(item => {
        return deleteTableRelationshipRequestBody({
          driver: getDriver(item.source) ?? '',
          name: item.name,
          source: item.source,
          table: item.table,
          isRemote: isRemoteRelationship(item.source, item.table, item.name),
        });
      });

      mutate(
        {
          query: {
            type: 'bulk_atomic',
            args: payloads,
            resource_version,
          },
        },
        options
      );
    },
    [getDriver, isRemoteRelationship, mutate, resource_version]
  );

  return {
    createTableRelationships,
    renameRelationships,
    deleteRelationships,
    ...rest,
  };
};
