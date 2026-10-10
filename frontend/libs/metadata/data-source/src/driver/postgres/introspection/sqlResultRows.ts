import type { RunSQLResponse } from '@hasura/shared/types';

/**
 * Turn a `run_sql` tuples result (`[headers, ...rows]`, every value as text)
 * into one object per row keyed by column name. Pure => cycle-free.
 */
export const sqlResultToRows = <TColumn extends string>(
  result: RunSQLResponse['result'] | undefined,
): Partial<Record<TColumn, string>>[] => {
  if (!result || result.length < 2) return [];
  const [headers, ...rows] = result;
  return rows.map((row) =>
    Object.fromEntries(headers.map((header, i) => [header, row[i]])),
  ) as Partial<Record<TColumn, string>>[];
};

/**
 * Parse a one-dimensional Postgres array literal in text output format, e.g.
 * `{id,"first name","a\"b"}` → `['id', 'first name', 'a"b']`. Unquoted `NULL`
 * elements are dropped. Returns `[]` for missing or malformed input.
 */
export const parsePostgresTextArray = (value: string | undefined): string[] => {
  if (!value || value[0] !== '{' || value[value.length - 1] !== '}') return [];

  const body = value.slice(1, -1);
  const items: string[] = [];
  let i = 0;

  while (i < body.length) {
    let item = '';
    if (body[i] === '"') {
      i++;
      while (i < body.length && body[i] !== '"') {
        if (body[i] === '\\') i++;
        item += body[i] ?? '';
        i++;
      }
      i++; // closing quote
      items.push(item);
    } else {
      while (i < body.length && body[i] !== ',') {
        item += body[i];
        i++;
      }
      if (item !== 'NULL') items.push(item);
    }
    i++; // separating comma
  }

  return items;
};
