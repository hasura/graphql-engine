// NOTE: It can be seen that the `name` field within the object that contains the
//       information about the inconsistent object for `remote_schema` and `remote_schema_permission`
//       contains a sentence with complete details of the remote schema(like name, role .etc). In here,
//       the name of the remote schema always comes at the very end. Since this "HACK" is being

import { InconsistentObject } from '@hasura/shared/types';

//       used to fetch the remote schema name, it can become a source of bugs.
export const getRemoteSchemaNameFromInconsistentObjects = (
  inconsistentObjects: InconsistentObject[],
) =>
  inconsistentObjects.reduce((rsNameList, inconsistentObject) => {
    if (
      'type' in inconsistentObject &&
      (inconsistentObject.type === 'remote_schema' ||
        inconsistentObject.type === 'remote_schema_permission')
    ) {
      const inconsistentObjectSplited = inconsistentObject.name?.split(' ');
      const rsName =
        inconsistentObjectSplited?.[inconsistentObjectSplited?.length - 1];
      if (!rsNameList.includes(rsName)) {
        // to avoid duplicate remote schema name
        return [...rsNameList, rsName];
      }
    } else if (
      'type' in inconsistentObject &&
      inconsistentObject?.type === 'remote_relationship' &&
      inconsistentObject?.definition?.remote_schema
    ) {
      if (!rsNameList.includes(inconsistentObject.definition.remote_schema)) {
        return [...rsNameList, inconsistentObject.definition.remote_schema];
      }
    }

    return rsNameList;
  }, [] as string[]);

// NOTE: for a inconsistent object of type "source" the inconsistentObject.definition is the name of the source
//       for every other inconsistent object if "source" is relevent it will be in inconsistentObject.definition.source

// getSourceFromInconsistentObjects should be used to extract the source from any inconsistent object
export const getSourceFromInconsistentObjects = (
  inconsistentObjects: InconsistentObject[],
) =>
  [
    ...new Set(
      inconsistentObjects
        .map((inconsistentObject) => {
          if (
            !inconsistentObject ||
            !('definition' in inconsistentObject) ||
            !inconsistentObject.definition
          ) {
            return null;
          }

          if (
            typeof inconsistentObject.definition === 'object' &&
            'source' in inconsistentObject.definition
          ) {
            return inconsistentObject.definition.source;
          }

          if (
            inconsistentObject.type === 'source' &&
            typeof inconsistentObject.definition === 'string'
          ) {
            return inconsistentObject.definition;
          }

          return null;
        })
        .filter(Boolean),
    ),
  ] as string[];
