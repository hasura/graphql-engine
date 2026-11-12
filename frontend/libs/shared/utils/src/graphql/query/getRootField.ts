import {
  getNamedType,
  GraphQLField,
  GraphQLObjectType,
  isObjectType,
} from 'graphql';
import { camelize } from 'inflection';

export type RootFieldMap = Record<string, GraphQLField<unknown, unknown>>;

/**
 * Returns the fields of a root operation type, looking inside the source's
 * root field namespace when one is set.
 */
export const getRootFieldMap = (
  rootType: GraphQLObjectType | null | undefined,
  namespace: string | undefined,
): RootFieldMap => {
  if (!rootType) return {};
  const fields = rootType.getFields();
  if (!namespace) return fields;

  const namespaceType = fields[namespace]
    ? getNamedType(fields[namespace].type)
    : undefined;
  return isObjectType(namespaceType) ? namespaceType.getFields() : {};
};

// `graphql-default` naming convention camel-cases the generated root fields,
// e.g. `insert_album` -> `insertAlbum`.
export const findRootField = (fields: RootFieldMap, name: string) =>
  fields[name] ?? fields[camelize(name, true)];
