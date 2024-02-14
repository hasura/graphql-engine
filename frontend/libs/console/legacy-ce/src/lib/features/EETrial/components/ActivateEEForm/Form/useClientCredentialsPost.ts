import * as React from 'react';
import { useMutation } from 'react-query';
import { useAppSelector } from '../../../../../storeHooks';
import { Api } from '../../../../../hooks/apiUtils';
import Endpoints from '../../../../../Endpoints';

type MutationArgs = {
  clientId: string;
  clientSecret: string;
  adminSecret: string;
};

const postClientCreds = (
  clientId: string,
  clientSecret: string,
  adminSecretHeader: Record<string, string>
) => {
  // doing this to remove content-type from the data headers
  return Api.post({
    headers: adminSecretHeader,
    url: Endpoints.license,
    body: {
      client_id: clientId,
      client_secret: clientSecret,
    },
  });
};

export const useClientCredentialsPost = (
  onSuccess?: VoidFunction,
  onError?: (msg: string) => void
) => {
  const headers = useAppSelector(state => state.tables.dataHeaders);
  const { mutate, isLoading } = useMutation(
    (args: MutationArgs) =>
      postClientCreds(args.clientId, args.clientSecret, headers),
    {
      onSuccess: () => {
        if (onSuccess) {
          onSuccess();
        }
      },
      onError: (e: any) => {
        if (onError && e?.message) {
          onError(e.message);
        }
      },
    }
  );

  return {
    post: mutate,
    isLoading,
  };
};
