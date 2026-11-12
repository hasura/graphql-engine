const NEEDS_QUOTES = /^$|^null$|[{}",\\\s]/i;

const formatElement = (value: unknown): string => {
  if (value === null || value === undefined) return 'NULL';
  if (Array.isArray(value)) return formatPostgresArrayLiteral(value);

  const text =
    typeof value === 'object' ? JSON.stringify(value) : String(value);
  return NEEDS_QUOTES.test(text)
    ? `"${text.replace(/\\/g, '\\\\').replace(/"/g, '\\"')}"`
    : text;
};

/**
 * Formats values as a Postgres array literal, e.g. `['a b', null]` ->
 * `{"a b",NULL}`. Elements that need it are quoted and escaped.
 */
export const formatPostgresArrayLiteral = (values: unknown[]): string =>
  `{${values.map(formatElement).join(',')}}`;

/**
 * Parses a one-dimensional Postgres array literal, e.g. `{a,"b c",NULL}` ->
 * `['a', 'b c', null]`. Returns `undefined` when the text isn't one.
 */
export const parsePostgresArrayLiteral = (
  literal: string,
): (string | null)[] | undefined => {
  const text = literal.trim();
  if (!text.startsWith('{') || !text.endsWith('}')) return undefined;

  const body = text.slice(1, -1);
  if (!body.trim()) return [];

  const values: (string | null)[] = [];
  let current = '';
  let quoted = false;
  let wasQuoted = false;

  for (let i = 0; i < body.length; i += 1) {
    const char = body[i];
    if (quoted) {
      if (char === '\\') {
        current += body[i + 1] ?? '';
        i += 1;
      } else if (char === '"') {
        quoted = false;
      } else {
        current += char;
      }
    } else if (char === '"') {
      quoted = true;
      wasQuoted = true;
    } else if (char === ',') {
      values.push(
        !wasQuoted && current.trim().toUpperCase() === 'NULL'
          ? null
          : wasQuoted
            ? current
            : current.trim(),
      );
      current = '';
      wasQuoted = false;
    } else if (char === '{' || char === '}') {
      // nested arrays aren't supported
      return undefined;
    } else {
      current += char;
    }
  }

  if (quoted) return undefined;
  values.push(
    !wasQuoted && current.trim().toUpperCase() === 'NULL'
      ? null
      : wasQuoted
        ? current
        : current.trim(),
  );
  return values;
};
