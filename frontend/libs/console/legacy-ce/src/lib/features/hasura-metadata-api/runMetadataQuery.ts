import { AxiosInstance } from 'axios';
import { useCallback } from 'react';
import { useHttpClient } from '../Network';

export const runMetadataQuery = async <ResponseType>({
  httpClient,
  body,
}: {
  body: Record<string, any>;
  httpClient: AxiosInstance;
}): Promise<ResponseType> => {
  return (await httpClient.post('/v1/metadata', body)).data;
};

// packages up everything so httpclient comes along for the ride
export const useRunMetadataQuery = <ResponseType>() => {
  const httpClient = useHttpClient();
  const runQuery = useCallback(
    (body: Record<string, any>) => {
      runMetadataQuery<ResponseType>({ httpClient, body });
    },
    [httpClient]
  );

  return {
    runQuery,
  };
};
