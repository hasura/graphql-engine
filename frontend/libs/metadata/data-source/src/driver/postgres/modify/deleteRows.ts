import { transformWhereBoolExp } from '../../common/validation';
import { DeleteRowProps } from '../../types';

export const deleteRows = async ({
  args,
  fetchJson,
  endpoints,
}: DeleteRowProps): Promise<number> => {
  const where = transformWhereBoolExp(args.where, args.columns, false);
  const result = await fetchJson(endpoints.queryV2, {
    method: 'POST',
    body: JSON.stringify({
      type: 'delete',
      args: {
        where,
        source: args.source.name,
        table: args.table,
      },
    }),
  });

  if (result && typeof result === 'object' && 'affected_rows' in result) {
    return result.affected_rows;
  }

  return 0;
};
