import { MetadataTable, Source } from '../../hasura-metadata-types';
import { AllowedQueryOperation } from '../query';
import { TableEntry } from '../../../metadata/types';

export const getTypeName = ({
  defaultQueryRoot,
  configuration,
  operation,
  sourceCustomization,
}: {
  defaultQueryRoot: string | never[];
  configuration?: TableEntry['configuration'] | MetadataTable['configuration'];
  operation: AllowedQueryOperation;
  sourceCustomization?: Source['customization'];
  defaultSchema?: string;
}): string => {
  /**
   *  Priority: Check if the table has a custom name set. If so, use that for query root.
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
  return `${sourceCustomization?.type_names?.prefix ?? ''}${baseQueryRoot}${
    sourceCustomization?.type_names?.suffix ?? ''
  }`;
};
