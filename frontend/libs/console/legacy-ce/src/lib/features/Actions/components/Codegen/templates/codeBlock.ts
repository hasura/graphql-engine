// Template tag equivalent to `codeBlock` from common-tags, which the
// typescript-express, javascript-js-doc-express and go-serve-mux templates
// were originally written with:
// - a multi-line substitution is inlined at the indentation of the line it is
//   substituted on
// - null, undefined, NaN and boolean substitutions render as nothing
// - the common indentation of the result is stripped, then it is trimmed

const isPrintable = (value: unknown) =>
  value !== null &&
  value !== undefined &&
  !Number.isNaN(value) &&
  typeof value !== 'boolean';

const renderSubstitution = (value: unknown, preceding: string): string => {
  const lines =
    typeof value === 'string' && value.includes('\n')
      ? value.split('\n')
      : value;

  if (Array.isArray(lines)) {
    const indentation = preceding.match(/(\n?[^\S\n]+)$/);
    return lines.filter(isPrintable).join(indentation ? indentation[1] : ' ');
  }

  return isPrintable(lines) ? String(lines) : '';
};

const stripIndent = (text: string) => {
  const indents = text.match(/^[^\S\n]*(?=\S)/gm);
  const minIndent = indents && Math.min(...indents.map((i) => i.length));
  return minIndent
    ? text.replace(new RegExp(`^.{${minIndent}}`, 'gm'), '')
    : text;
};

export const codeBlock = (
  strings: TemplateStringsArray,
  ...values: unknown[]
): string => {
  const result = values.reduce<string>(
    (acc, value, i) => acc + renderSubstitution(value, acc) + strings[i + 1],
    strings[0],
  );
  return stripIndent(result).trim();
};
