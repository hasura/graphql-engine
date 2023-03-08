import { MetadataTable, Source } from '../../hasura-metadata-types';

export type AllowedMutationOperation =
  | 'insert'
  | 'insert_one'
  | 'update'
  | 'update_by_pk'
  | 'update_many'
  | 'delete'
  | 'delete_by_pk';

export const getMutationRoot = ({
  defaultQueryRoot,
  configuration,
  operation,
  sourceCustomization,
}: {
  defaultQueryRoot: string;
  configuration?: MetadataTable['configuration'];
  operation: AllowedMutationOperation;
  sourceCustomization?: Source['customization'];
}): string => {
  /**
   * Priority 1: Check if `operation` has a custom name. If so, use that for the query root.
   * Also there is no need to suffix operation names to the name here, since you're replacing the entire query root name
   * and can uniquely identify an operation query
   */
  const customRootName = configuration?.custom_root_fields?.[operation];

  if (customRootName)
    return `${sourceCustomization?.root_fields?.prefix ?? ''}${customRootName}${
      sourceCustomization?.root_fields?.suffix ?? ''
    }`;

  /**
   *  Priority 2: Check if the table has a custom name set. If so, use that for query root.
   *  (OR)
   *  Default: Just use the table name itself as the query root.
   *  Note: Postgres's `public` schema tables' query roots are not prefixed with the schema name in the final
   *  generated GraphQL schema.
   */
  let baseQueryRoot = defaultQueryRoot;

  if (configuration?.custom_name) baseQueryRoot = configuration?.custom_name;

  /**
   * for `select_by_pk` and `select_aggregate` the following suffixes are required if there is no custom field name set
   */
  if (operation === 'insert') baseQueryRoot = `insert_${baseQueryRoot}`;
  if (operation === 'insert_one') baseQueryRoot = `insert_${baseQueryRoot}_one`;

  if (operation === 'update') baseQueryRoot = `update_${baseQueryRoot}`;
  if (operation === 'update_by_pk')
    baseQueryRoot = `update_${baseQueryRoot}_by_pk`;
  if (operation === 'update_many')
    baseQueryRoot = `update_${baseQueryRoot}_many`;

  if (operation === 'delete_by_pk')
    baseQueryRoot = `delete_${baseQueryRoot}_by_pk`;

  if (operation === 'delete') baseQueryRoot = `delete_${baseQueryRoot}`;

  /**
   * The `select` operation has no operation suffix.
   * `sourceCustomization` is independent of the table level GQL customization - prefix/suffix are to be added to the `queryRoot` finally.
   */
  return `${sourceCustomization?.root_fields?.prefix ?? ''}${baseQueryRoot}${
    sourceCustomization?.root_fields?.suffix ?? ''
  }`;
};
