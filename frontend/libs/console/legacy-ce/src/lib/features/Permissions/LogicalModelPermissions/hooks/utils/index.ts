export function mapPostgresToPg(kind: string) {
  return kind === 'postgres' ? 'pg' : kind;
}
