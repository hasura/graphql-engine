import { AxiosInstance } from 'axios';

export const runMetadataQuery = async <ResponseType>({
  httpClient,
  body,
}: {
  body: Record<string, any>;
  httpClient: AxiosInstance;
}): Promise<ResponseType> => {
  return (await httpClient.post('/v1/metadata', body)).data;
};
