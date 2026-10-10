import { LatestReleaseVersionResult } from '@hasura/shared/types';
import { requestJson } from '@hasura/shared/utils';

export const loadServerVersion = (url: string) => {
  const options = {
    method: 'GET',
  };

  return requestJson<{ version: string }>(url, options);
};

export const loadLatestServerVersion = (
  baseUrl: string,
  serverVersion: string,
) => {
  const url = `${baseUrl}?agent=console&version=${serverVersion}`;
  const options = {
    method: 'GET',
  };

  return requestJson<LatestReleaseVersionResult>(url, options);
};
