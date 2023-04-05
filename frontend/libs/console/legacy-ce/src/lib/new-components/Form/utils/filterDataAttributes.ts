export function filterDataAttributes<T>(
  obj: Record<string, T>
): Record<string, T> {
  const result: Record<string, T> = {};
  for (const key in obj) {
    if (key.startsWith('data-')) {
      result[key] = obj[key];
    }
  }
  return result;
}
