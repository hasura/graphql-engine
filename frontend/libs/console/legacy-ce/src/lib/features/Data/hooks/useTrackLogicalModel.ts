import { transformErrorResponse } from '../../ConnectDBRedesign/utils';
import { useMetadataMigration } from '../../MetadataAPI';
import { MetadataMigrationOptions } from '../../MetadataAPI/hooks/useMetadataMigration';
import { useMetadata } from '../../hasura-metadata-api';
import { LogicalModel, Source } from '../../hasura-metadata-types';
import {
  LogicalModelMigrationBuilder,
  NativeQueryMigrationBuilder,
  StoredProcedureMigrationBuilder,
} from '../LogicalModels/MigrationBuilder';
import { findReferencedEntities } from '../LogicalModels/LogicalModel/utils/findReferencedEntities';
import { getSourceDriver } from './utils';

export const getTrackLogicalModelPayload = ({
  data: { dataSourceName, name, fields },
  editDetails,
  sources,
}: TrackLogicalModelArgs & { sources: Source[] }):
  | never
  | Record<string, unknown>[] => {
  const driver = getSourceDriver(sources, dataSourceName);

  if (!driver) {
    throw new Error('Source could not be found. Unable to identify driver.');
  }

  const logicalModel = { name, fields };

  const builder = new LogicalModelMigrationBuilder({
    dataSourceName,
    driver,
    logicalModel,
  });

  // create initial migration payload for the logical model:
  let payload: Record<string, unknown>[] = editDetails
    ? builder.untrack(editDetails.originalName).track().payload()
    : builder.track().payload();

  // check if the name was changed...
  if (editDetails && editDetails.originalName !== logicalModel.name) {
    // !!!!!!
    // the logical model name has been changed so the migration is more complicated
    // !!!!!!

    //1. first find any entities that refer to this logical model:
    const { native_queries, stored_procedures, logical_models, count } =
      findReferencedEntities({
        source: sources.find(s => s.name === dataSourceName),
        logicalModelName: editDetails.originalName,
      });

    if (count <= 0) {
      return payload;
    }

    // if there are any other entities that refer to this logical model:
    // migrate any stored procedures whose return type is THIS logical model
    const spMigrations = stored_procedures
      .map(p => {
        const migration = new StoredProcedureMigrationBuilder({
          dataSourceName,
          driver,
          storedProcedure: p,
        });
        migration.untrack();
        migration.updateLogicalModel(logicalModel.name);
        migration.track();
        return migration.payload();
      })
      .flat();

    // migrate any native query whose return type is THIS logical model
    const nqMigrations = native_queries
      .map(q => {
        const migration = new NativeQueryMigrationBuilder({
          dataSourceName,
          driver,
          nativeQuery: q,
        });
        migration.untrack();
        migration.updateLogicalModel(logicalModel.name);
        migration.track();
        return migration.payload();
      })
      .flat();

    // migrate any logical model fields that refer to THIS logical model
    const lmMigrations = logical_models
      .map(result => {
        const migration = new LogicalModelMigrationBuilder({
          dataSourceName,
          driver,
          logicalModel: result.logicalModel,
        });
        migration.untrack();
        migration
          .updateFieldsLogicalModelReference({
            currentName: editDetails.originalName,
            newName: logicalModel.name,
          })
          .track();
        return migration.payload();
      })
      .flat();

    payload = [...payload, ...spMigrations, ...nqMigrations, ...lmMigrations];

    return payload;
  }

  return payload;
};

export type TrackLogicalModel = {
  dataSourceName: string;
} & LogicalModel;

export type TrackLogicalModelArgs = {
  data: TrackLogicalModel;
  editDetails?: { originalName: string };
} & MetadataMigrationOptions;

export const useTrackLogicalModel = (
  globalMutateOptions?: MetadataMigrationOptions
) => {
  /**
   * Get the required metadata variables - sources & resource_version
   */
  const { data: { sources = [], resource_version } = {} } = useMetadata(m => ({
    sources: m.metadata.sources,
    resource_version: m.resource_version,
  }));

  const { mutate, ...rest } = useMetadataMigration({
    ...globalMutateOptions,
    errorTransform: transformErrorResponse,
    onSuccess: (data, variable, ctx) => {
      globalMutateOptions?.onSuccess?.(data, variable, ctx);
    },
  });

  const trackLogicalModel = async ({
    data: { dataSourceName, name, fields },
    editDetails,
    ...options
  }: TrackLogicalModelArgs) => {
    const payload = getTrackLogicalModelPayload({
      data: { dataSourceName, name, fields },
      editDetails,
      sources,
    });

    if (payload) {
      mutate(
        {
          query: {
            resource_version,
            type: 'bulk_atomic',
            args: payload,
          },
        },
        options
      );
    }
  };

  // this will fail if anythin references it
  const untrackLogicalModel = async ({
    data: { dataSourceName, name, dataSourceKind },
    ...options
  }: {
    data: { dataSourceName: string; name: string; dataSourceKind: string };
  } & MetadataMigrationOptions) => {
    mutate(
      {
        query: {
          resource_version,
          type: `${dataSourceKind}_untrack_logical_model`,
          args: {
            source: dataSourceName,
            name,
          },
        },
      },
      options
    );
  };

  return {
    trackLogicalModel,
    untrackLogicalModel,
    getTrackLogicalModelPayload,
    ...rest,
  };
};
