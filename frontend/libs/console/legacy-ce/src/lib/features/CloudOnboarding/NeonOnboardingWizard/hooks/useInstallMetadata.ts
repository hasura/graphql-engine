import { useCallback } from 'react';
import { FetchJson, isJsonString } from '@hasura/shared/utils';
import { useMutation, useQuery } from '@tanstack/react-query';
import { staleTime } from '../../constants';
import {
  fetchTemplateDataQueryFn,
  getHttpErrorMessage,
  transformOldMetadata,
} from '../utils';
import { useAppContext } from '@hasura/shared/context';
import { HasuraMetadataV3, Metadata } from '@hasura/shared/types';
import { runMetadataQuery, useMetadata } from '@hasura/metadata/api';
import { useAuthFetchJson } from '@hasura/shared/hooks';

type MutationFnArgs = {
  url: string;
  fetchJson: FetchJson;
  newMetadata: HasuraMetadataV3;
};

/**
 * Mutation Function to install the metadata. Calls the `replace_metadata` api with the new
 * metadata to be replaced. Then calls the `reload_meatadata` api to get graphql engine in sync
 * with the latest matadata.
 */
const installMetadataMutationFn = async (args: MutationFnArgs) => {
  const { newMetadata, fetchJson, url } = args;

  const replaceMetadataPayload = {
    type: 'replace_metadata' as const,
    args: newMetadata,
  };

  await runMetadataQuery({
    url,
    fetchJson,
    body: replaceMetadataPayload,
  });

  const reloadMetadataPayload = {
    type: 'reload_metadata' as const,
    args: {
      reload_sources: true,
    },
  };

  await runMetadataQuery({
    url,
    fetchJson,
    body: reloadMetadataPayload,
  });
};

/**
 * Hook to install metadata from a remote file containing hasura metadata. This will append the new metadata
 * to the provided data source
 * @returns A memoised function which can be called imperatively to apply the metadata
 */
export function useInstallMetadata(
  dataSourceName: string,
  metadataFileUrl: string,
  onSuccessCb?: () => void,
  onErrorCb?: (errorMsg?: string) => void,
): { updateMetadata: () => void } | { updateMetadata: undefined } {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const { data: oldMetadata } = useMetadata();

  // Fetch the metadata to be applied from remote file, or return from react-query cache if present
  const {
    data: templateMetadata,
    isLoading,
    isError,
  } = useQuery({
    queryKey: [metadataFileUrl],
    queryFn: () => fetchTemplateDataQueryFn<string>(metadataFileUrl, {}),
    staleTime,
  });

  const mutation = useMutation({
    mutationFn: (args: MutationFnArgs) => installMetadataMutationFn(args),
    onSuccess: onSuccessCb,
    onError: (error: Error) => {
      if (onErrorCb) {
        onErrorCb(getHttpErrorMessage(error) ?? 'Failed to apply metadata');
      }
    },
  });

  // only do a 'replace_metadata' call if we have the new metadata from the remote url, and current metadata is not null.
  // otherwise `updateMetadata` will just return an empty function. In that case, error callbacks will have info on what went wrong.
  const updateMetadata = useCallback(() => {
    if (templateMetadata && oldMetadata) {
      let templateMetadataJson: HasuraMetadataV3 | undefined;
      if (isJsonString(templateMetadata)) {
        templateMetadataJson = (JSON.parse(templateMetadata) as Metadata)
          ?.metadata as HasuraMetadataV3;
      }
      if (templateMetadataJson) {
        const transformedMetadata = transformOldMetadata(
          oldMetadata.metadata,
          templateMetadataJson,
          dataSourceName,
        );

        mutation.mutate({
          url: endpoints.metadata,
          fetchJson,
          newMetadata: transformedMetadata,
        });
      }
    }
    // not adding mutation to dependencies as its a non-memoised function, will trigger this useCallback
    // every time we do a mutation. https://github.com/TanStack/query/issues/1858
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [oldMetadata, templateMetadata, dataSourceName]);

  if (isError) {
    if (onErrorCb) {
      onErrorCb(
        `Failed to fetch metadata from the provided Url: ${metadataFileUrl}`,
      );
    }
    return { updateMetadata: undefined };
  }

  if (isLoading) {
    return { updateMetadata: undefined };
  }

  return { updateMetadata };
}
