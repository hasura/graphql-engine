import { AxiosInstance } from 'axios';
import { Metadata } from '../hasura-metadata-types';

export const exportMetadata = async ({
  httpClient,
}: {
  httpClient: AxiosInstance;
}): Promise<Metadata> => {
  return (
    await httpClient.post('/v1/metadata', {
      type: 'export_metadata',
      version: 2,
      args: {},
    })
  ).data;
};
