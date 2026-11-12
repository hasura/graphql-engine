import { DeleteRowProps } from '../../types';
import { transformWhereBoolExp } from '../validation';
import {
  GetDefaultQueryRoot,
  runRowMutationFromGraphQL,
} from './runRowMutation';

/**
 * The common deleteRows function uses GraphQL to delete rows.
 */
export const deleteRowsCurry =
  (getDefaultQueryRoot: GetDefaultQueryRoot) =>
  async ({ args, fetchJson, endpoints }: DeleteRowProps): Promise<number> => {
    const { affectedRows } = await runRowMutationFromGraphQL({
      endpoints,
      fetchJson,
      source: args.source,
      table: args.table,
      getDefaultQueryRoot,
      operation: 'delete',
      where: transformWhereBoolExp(args.where, args.columns, false),
    });

    return affectedRows;
  };
