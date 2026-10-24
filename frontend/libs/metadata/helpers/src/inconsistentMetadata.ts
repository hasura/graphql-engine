import {
  InconsistentObject,
  InconsistentObjectFields,
  InconsistentObjectRemoteSchema,
  InconsistentSource,
  Table,
} from '@hasura/shared/types';
import { areTablesEqual, areTablesEqualCoalesce } from './table';

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

/**
 * Finds the inconsistency of a tracked table (or view) or function. The
 * server reports its qualified name as the `definition`, and its source only
 * in the `name`, e.g. `table foobar in source default`.
 */
export const findInconsistentSourceObject = (
  inconsistentObjects: InconsistentObject[] | undefined,
  {
    source,
    type,
    definition,
  }: { source: string; type: 'table' | 'function'; definition: unknown },
): InconsistentObjectFields | undefined =>
  inconsistentObjects?.find(
    (obj): obj is InconsistentObjectFields =>
      'type' in obj &&
      obj.type === type &&
      obj.name.endsWith(` in source ${source}`) &&
      (areTablesEqual(obj.definition as Table, definition as Table) ||
        !!areTablesEqualCoalesce(obj.definition, definition)),
  );
