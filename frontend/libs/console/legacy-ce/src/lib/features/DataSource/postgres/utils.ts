function containsUppercase(str: string) {
  return /[A-Z]/.test(str);
}

export function adaptStringForPostgres(str: string) {
  return containsUppercase(str) ? `"${str}"` : str;
}
