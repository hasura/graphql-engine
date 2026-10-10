import { hasuraToast } from '@hasura/shared/ui';
import { getDriverPrefix } from '@hasura/metadata/helpers';
import {
  MetadataMigrationOptions,
  useMetadataHelpers,
  useMetadataMigration,
} from '../metadata';
import { SupportedDriver, Table } from '@hasura/shared/types';
import { useErrorNotification } from '../notification';

export type DropRelationshipArgs = {
  // Name of the relationship that needs to be dropped.
  relationship: string;
  // When set to true, all the dependent items on this relationship are also dropped.
  cascade?: boolean;
  // Name of the source database of the table (default: default)
  source: string;
  // Name of the table
  table: Table;
};

export const getDropRelationshipType = (driver: SupportedDriver) => {
  const prefix = getDriverPrefix(driver!);
  return `${prefix}_drop_relationship` as const;
};

/**
 * useDropRelationship is used to drop a relationship (both object and array) on a table.
 * If there are other objects dependent on this relationship like permissions and query templates, etc.,
 * the request will fail and report the dependencies unless cascade is set to true.
 * If cascade is set to true, the dependent objects are also dropped.
 */
export const useDropRelationship = () => {
  const { mutate, ...rest } = useMetadataMigration();
  const { fetchSource } = useMetadataHelpers();
  const showErrorNotification = useErrorNotification();

  const dropRelationship = async (
    args: DropRelationshipArgs,
    options?: MetadataMigrationOptions,
  ) => {
    const { source, resource_version } = await fetchSource(args.source);
    const requestBody = {
      type: getDropRelationshipType(source.kind),
      args,
      resource_version,
    };

    return mutate(
      {
        query: requestBody,
      },
      {
        onSuccess: (data, variables, onMutateResult, context) => {
          hasuraToast({
            title: 'Success!',
            message: 'Relationship deleted successfully',
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
    dropRelationship,
    ...rest,
  };
};
