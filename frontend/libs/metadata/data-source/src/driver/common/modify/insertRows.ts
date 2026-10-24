import { InsertRowProps, ValidateInputRowValuesFunction } from '../../types';
import {
  GetDefaultQueryRoot,
  runRowMutationFromGraphQL,
} from './runRowMutation';

/**
 * The common insertRows function uses GraphQL to insert rows.
 */
export const insertRowsCurry =
  (
    validate: ValidateInputRowValuesFunction,
    getDefaultQueryRoot: GetDefaultQueryRoot,
  ) =>
  async ({ args, fetchJson, endpoints }: InsertRowProps): Promise<number> => {
    const { affectedRows } = await runRowMutationFromGraphQL({
      endpoints,
      fetchJson,
      source: args.source,
      table: args.table,
      getDefaultQueryRoot,
      operation: 'insert',
      objects: args.objects.map((values) =>
        validate({ columns: args.columns, graphqlMode: false, values }),
      ),
    });

    return affectedRows;
  };
