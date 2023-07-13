import { transformErrorResponse } from '../../../ConnectDBRedesign/utils';
import { useMetadataMigration } from '../../../MetadataAPI';
import { MetadataMigrationOptions } from '../../../MetadataAPI/hooks/useMetadataMigration';
import { MetadataSelectors, useMetadata } from '../../../hasura-metadata-api';
import {
  NativeQuery,
  NativeQueryRelationship,
  Source,
} from '../../../hasura-metadata-types';
import { NativeQueryMigrationBuilder } from '../../LogicalModels/MigrationBuilder';

export type TrackNativeQueryRelationshipsProps = NativeQueryRelationship & {
  type: 'object' | 'array';
};

export type UntrackNativeQuery = { source: Source } & Pick<
  NativeQuery,
  'root_field_name'
>;

export const useTrackNativeQueryRelationships = (
  dataSourceName: string,
  nativeQueryName: string,
  globalMutateOptions?: MetadataMigrationOptions
) => {
  /**
   * Get the required metadata variables - sources & resource_version
   */
  const { data: { driver, originNativeQuery, resource_version } = {} } =
    useMetadata(m => ({
      driver: MetadataSelectors.findSource(dataSourceName)(m)?.kind,
      originNativeQuery: MetadataSelectors.findSource(dataSourceName)(
        m
      )?.native_queries?.find(nq => nq.root_field_name === nativeQueryName),
      resource_version: m.resource_version,
    }));

  const { mutate, ...rest } = useMetadataMigration({
    ...globalMutateOptions,
    errorTransform: transformErrorResponse,
    onSuccess: (data, variable, ctx) => {
      globalMutateOptions?.onSuccess?.(data, variable, ctx);
    },
  });

  const trackNativeQueryRelationship = async ({
    data: args,
    editDetails,
    ...options
  }: {
    data: TrackNativeQueryRelationshipsProps;
    // these are the original name/type before edit.
    // we need these b/c if the user alters either one, we have to do some special handling
    editDetails?: { name: string; type: 'object' | 'array' };
  } & MetadataMigrationOptions) => {
    if (!driver || !originNativeQuery)
      throw Error('Driver/Native Query not found');

    const { type, ...relationshipDetails } = args;

    const nativeQueryPayload = new NativeQueryMigrationBuilder({
      driver,
      nativeQuery: originNativeQuery,
      dataSourceName,
    }).untrack();

    if (editDetails) {
      // if editing, remove the relationship matching the original name and type prior to edit
      // if a user changed the type or name, we need the originals to make sure we are dropping the right object from the correct array of relationships
      nativeQueryPayload.removeRelationship(editDetails.type, editDetails.name);
    }

    nativeQueryPayload
      // add a relationship to the native query with the details passed in
      .addRelationship(type, relationshipDetails)
      // add a track call
      .track();

    mutate(
      {
        query: {
          resource_version,
          type: `bulk_atomic`,
          args: nativeQueryPayload.payload(),
        },
      },
      options
    );
  };

  const untrackNativeQueryRelationship = async ({
    data: { name: relationshipName, type },
    ...options
  }: {
    data: Pick<TrackNativeQueryRelationshipsProps, 'name' | 'type'>;
  } & MetadataMigrationOptions) => {
    if (!driver || !originNativeQuery)
      throw Error('Driver/Native Query not found');

    const relationship = originNativeQuery[`${type}_relationships`]?.find(
      n => n.name === relationshipName
    );

    if (!relationship) {
      throw new Error('Unable to find relationship');
    }

    const nativeQueryPayload = new NativeQueryMigrationBuilder({
      driver,
      nativeQuery: originNativeQuery,
      dataSourceName,
    })
      .untrack()
      .removeRelationship(type, relationshipName)
      .track();

    mutate(
      {
        query: {
          resource_version,
          type: `bulk_atomic`,
          args: nativeQueryPayload.payload(),
        },
      },
      options
    );
  };

  return {
    trackNativeQueryRelationship,
    untrackNativeQueryRelationship,
    ...rest,
  };
};
