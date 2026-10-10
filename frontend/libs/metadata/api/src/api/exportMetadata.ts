import type { Metadata } from '@hasura/shared/types';
import { NetworkArgs } from '../types';

const body = JSON.stringify({
  type: 'export_metadata',
  version: 2,
  args: {},
});

export const exportMetadata = async ({
  url,
  fetchJson,
}: NetworkArgs): Promise<Metadata> => {
  return fetchJson(url, {
    method: 'POST',
    body,
  });
};
