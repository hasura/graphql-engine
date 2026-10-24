import { UpdateRowProps, ValidateInputRowValuesFunction } from '../../types';
import { transformWhereBoolExp } from '../validation';
import {
  GetDefaultQueryRoot,
  runRowMutationFromGraphQL,
} from './runRowMutation';

/**
 * The common updateRows function uses GraphQL to update rows.
 */
export const updateRowsCurry =
  (
    validate: ValidateInputRowValuesFunction,
    getDefaultQueryRoot: GetDefaultQueryRoot,
  ) =>
  async ({ args, fetchJson, endpoints }: UpdateRowProps): Promise<number> => {
    const { affectedRows } = await runRowMutationFromGraphQL({
      endpoints,
      fetchJson,
      source: args.source,
      table: args.table,
      getDefaultQueryRoot,
      operation: 'update',
      set: validate({
        columns: args.columns,
        graphqlMode: false,
        values: args.set,
      }),
      where: transformWhereBoolExp(args.where, args.columns, false),
    });

    return affectedRows;
  };
