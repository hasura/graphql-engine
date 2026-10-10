export const getPathRoot = (path: string | undefined | null = '') =>
  path?.split('/')[1] ?? '';

export const stripTrailingSlash = (url: string) => {
  if (url && url.endsWith('/')) {
    return url.slice(0, -1);
  }

  return url ?? '';
};

export const isValidURL = (value: string) => {
  try {
    new URL(value);
  } catch {
    return false;
  }
  return true;
};

export const isURLTemplated = (value: string): boolean => {
  return /{{(\S+)}}/.test(value);
};

export const isValidTemplateLiteral = (literal_: string) => {
  const literal = literal_.trim();
  if (!literal) return false;
  const templateStartIndex = literal.indexOf('{{');
  const templateEndEdex = literal.indexOf('}}');
  return templateStartIndex !== -1 && templateEndEdex > templateStartIndex + 2;
};
