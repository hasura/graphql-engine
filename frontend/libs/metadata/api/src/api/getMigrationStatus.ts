import { requestJson } from '@hasura/shared/utils';

export function getMigrationStatus(url: string) {
  const options = {
    method: 'GET',
    headers: { 'content-type': 'application/json' },
  };

  return requestJson<{ migration_mode: 'true' }>(url, options).then((result) =>
    result?.migration_mode === 'true' ? 'healthy' : 'unhealthy',
  );
}
