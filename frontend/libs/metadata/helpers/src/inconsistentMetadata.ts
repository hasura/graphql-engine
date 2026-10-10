import {
  InconsistentObject,
  InconsistentObjectRemoteSchema,
  InconsistentSource,
} from '@hasura/shared/types';

export function findInconsistentRemoteSchema(
  inconsistentObjects: InconsistentObject[] | undefined,
  remoteSchemaName: string,
): InconsistentObjectRemoteSchema | undefined {
  return inconsistentObjects?.find(
    (obj) =>
      'type' in obj &&
      obj.type === 'remote_schema' &&
      obj.name === `remote_schema ${remoteSchemaName}`,
  ) as InconsistentObjectRemoteSchema | undefined;
}

export const findInconsistentSource = (
  inconsistentObjects: InconsistentObject[],
  sourceName: string,
): InconsistentSource | undefined =>
  inconsistentObjects.find(
    (obj) =>
      'type' in obj && obj.type === 'source' && sourceName === obj.definition,
  ) as InconsistentSource | undefined;
