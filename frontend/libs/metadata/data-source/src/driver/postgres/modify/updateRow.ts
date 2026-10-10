import { transformWhereBoolExp } from '../../common/validation';
import { UpdateRowProps } from '../../types';
import { validatePostgresInputRowValues } from '../utils';

export const updateRows = async ({
  args,
  fetchJson,
  endpoints,
}: UpdateRowProps): Promise<number> => {
  const values = validatePostgresInputRowValues({
    columns: args.columns,
    values: args.set,
    graphqlMode: false,
  });

  const where = transformWhereBoolExp(args.where, args.columns, false);
  const result = await fetchJson(endpoints.queryV2, {
    method: 'POST',
    body: JSON.stringify({
      type: 'update',
      args: {
        where,
        source: args.source.name,
        table: args.table,
        $set: values,
        $default: args.defaultColumns,
      },
    }),
  });

  if (result && typeof result === 'object' && 'affected_rows' in result) {
    return result.affected_rows;
  }

  return 0;
};
