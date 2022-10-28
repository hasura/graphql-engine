import { MetadataTable, Source } from '@/features/MetadataAPI';

export type AllowedQueryOperation =
  | 'select'
  | 'select_by_pk'
  | 'select_aggregate';
export const getQueryRoot = ({
  defaultQueryRoot,
  configuration,
  operation,
  sourceCustomization,
}: {
  defaultQueryRoot: string;
  configuration?: MetadataTable['configuration'];
  operation: AllowedQueryOperation;
  sourceCustomization?: Source['customization'];
  defaultSchema?: string;
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
  if (operation === 'select_by_pk') baseQueryRoot = `${baseQueryRoot}_by_pk`;

  if (operation === 'select_aggregate')
    baseQueryRoot = `${baseQueryRoot}_aggregate`;

  /**
   * The `select` operation has no operation suffix.
   * `sourceCustomization` is independent of the table level GQL customization - prefix/suffix are to be added to the `queryRoot` finally.
   */
  return `${sourceCustomization?.root_fields?.prefix ?? ''}${baseQueryRoot}${
    sourceCustomization?.root_fields?.suffix ?? ''
  }`;
};
