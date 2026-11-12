/** Format only whitespace so JSON numbers and string escapes stay lossless. */
export const formatJsonInput = (value: string, expanded: boolean): string => {
  try {
    JSON.parse(value);
  } catch {
    // Empty inputs and unfinished edits must survive toggling the editor.
    return value;
  }

  const tokens = value.match(/"(?:\\.|[^"\\])*"|[^\s]/g) ?? [];
  if (!expanded) return tokens.join('');

  let depth = 0;
  return tokens
    .map((token, index) => {
      const previous = tokens[index - 1];
      const next = tokens[index + 1];
      const newline = () => `\n${'  '.repeat(depth)}`;

      switch (token) {
        case '{':
        case '[':
          depth++;
          return token + (next === '}' || next === ']' ? '' : newline());
        case '}':
        case ']':
          depth--;
          return (
            (previous === '{' || previous === '[' ? '' : newline()) + token
          );
        case ',':
          return token + newline();
        case ':':
          return ': ';
        default:
          return token;
      }
    })
    .join('');
};
