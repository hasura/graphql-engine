import { useMutation } from '@tanstack/react-query';
import Endpoints from '../../../../../Endpoints';
import { useAuthContext } from '@hasura/shared/context';
import { requestJson } from '@hasura/shared/utils';

type MutationArgs = {
  clientId: string;
  clientSecret: string;
  adminSecret: string;
};

const postClientCreds = (
  clientId: string,
  clientSecret: string,
  adminSecretHeader: Record<string, string>,
) => {
  // doing this to remove content-type from the data headers
  return requestJson(Endpoints.license, {
    method: 'POST',
    headers: adminSecretHeader,
    body: JSON.stringify({
      client_id: clientId,
      client_secret: clientSecret,
    }),
  });
};

export const useClientCredentialsPost = (
  onSuccess?: VoidFunction,
  onError?: (msg: string) => void,
) => {
  const { getHeaders } = useAuthContext();
  const { mutate, isPending: isLoading } = useMutation({
    mutationFn: async (args: MutationArgs) => {
      const headers = await getHeaders();
      return postClientCreds(args.clientId, args.clientSecret, headers);
    },
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
  });

  return {
    post: mutate,
    isLoading,
  };
};
