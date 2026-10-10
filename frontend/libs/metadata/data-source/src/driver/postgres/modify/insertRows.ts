import { InsertRowProps } from '../../types';
import { validatePostgresInputRowValues } from '../utils';

export async function insertRows({
  args,
  fetchJson,
  endpoints,
}: InsertRowProps): Promise<number> {
  const objects = args.objects.map((obj) =>
    validatePostgresInputRowValues({
      columns: args.columns,
      values: obj,
      graphqlMode: false,
    }),
  );

  const result = (await fetchJson(endpoints.queryV2, {
    method: 'POST',
    body: JSON.stringify({
      type: 'insert',
      args: {
        objects,
        // returning,
        source: args.source.name,
        table: args.table,
      },
    }),
  })) as { affected_rows: number; returning: Array<Record<string, any>> };

  if (result && typeof result === 'object' && 'affected_rows' in result) {
    return result.affected_rows;
  }

  return 0;
}
