import { hasuraToast } from '@hasura/shared/ui';
import {
  MetadataMigrationOptions,
  useMetadataHelpers,
  useMetadataMigration,
} from '../metadata';
import {
  DropRelationshipArgs,
  getDropRelationshipType,
} from './useDropRelationship';
import {
  DeleteRemoteRelationshipArgs,
  getDeleteRemoteRelationshipType,
} from './useDeleteRemoteRelationship';
import { TMigrationSingleQuery } from '../../api';
import { Metadata } from '@hasura/shared/types';
import { areTablesEqual, MetadataSelectors } from '@hasura/metadata/helpers';
import { useErrorNotification } from '../notification';

type DropRelationshipsArgs = DropRelationshipArgs[];

/**
 * useDropRelationship is used to drop a relationship (both object and array) on a table.
 * If there are other objects dependent on this relationship like permissions and query templates, etc.,
 * the request will fail and report the dependencies unless cascade is set to true.
 * If cascade is set to true, the dependent objects are also dropped.
 */
export const useDropRelationships = () => {
  const { mutate, ...rest } = useMetadataMigration();
  const { fetchMetadata } = useMetadataHelpers();
  const showErrorNotification = useErrorNotification();

  const dropRelationships = async (
    inputs: DropRelationshipsArgs,
    options?: MetadataMigrationOptions,
  ) => {
    const meta = await fetchMetadata();

    return mutate(
      {
        query: {
          type: 'bulk_atomic',
          resource_version: meta.resource_version,
          args: inputs
            .map((item) => {
              const source = MetadataSelectors.findSource(item.source)(meta);
              if (!source) {
                return null;
              }

              const isRemote = isRemoteRelationship(meta.metadata, item);

              return isRemote
                ? {
                    type: getDeleteRemoteRelationshipType(source.kind),
                    args: {
                      name: item.relationship,
                      source: item.source,
                      table: item.table,
                    } as DeleteRemoteRelationshipArgs,
                  }
                : {
                    type: getDropRelationshipType(source.kind),
                    args: item,
                  };
            })
            .filter(Boolean) as TMigrationSingleQuery[],
        },
      },
      {
        onSuccess: (data, variables, onMutateResult, context) => {
          hasuraToast({
            title: 'Success!',
            message: 'Relationships deleted successfully',
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

  return {
    dropRelationships,
    ...rest,
  };
};

const isRemoteRelationship = (
  metadata: Metadata['metadata'],
  item: DropRelationshipArgs,
) => {
  return (
    metadata.sources
      .find((s) => s.name === item.source)
      ?.tables.find((t) => areTablesEqual(t.table, item.table))
      ?.remote_relationships?.some((r) => r.name === item.relationship) ?? false
  );
};
