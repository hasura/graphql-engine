import { useCallback } from 'react';
import Endpoints from '../../../../Endpoints';
import { Api } from '../../../../hooks/apiUtils';
import { isJsonString } from '../../../../components/Common/utils/jsUtils';
import { HasuraMetadataV3 } from '../../../../metadata/types';
import { MetadataResponse } from '../../../MetadataAPI';
import { useAppSelector } from '../../../../storeHooks';
import { useMutation, useQuery } from 'react-query';
import { staleTime } from '../../constants';
import { fetchTemplateDataQueryFn, transformOldMetadata } from '../utils';

type MutationFnArgs = {
  newMetadata: HasuraMetadataV3;
  headers: Record<string, string>;
};

/**
 * Mutation Function to install the metadata. Calls the `replace_metadata` api with the new
 * metadata to be replaced. Then calls the `reload_meatadata` api to get graphql engine in sync
 * with the latest matadata.
 */
const installMetadataMutationFn = async (args: MutationFnArgs) => {
  const { newMetadata, headers } = args;

  const replaceMetadataPayload = {
    type: 'replace_metadata',
    args: newMetadata,
  };
  await Api.post<Record<string, string>>({
    url: Endpoints.metadata,
    headers,
    body: replaceMetadataPayload,
  });

  const reloadMetadataPayload = {
    type: 'reload_metadata',
    args: {
      reload_sources: true,
    },
  };
  await Api.post<Record<string, string>>({
    url: Endpoints.metadata,
    headers,
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
  onErrorCb?: (errorMsg?: string) => void
): { updateMetadata: () => void } | { updateMetadata: undefined } {
  const headers = useAppSelector(state => state.tables.dataHeaders);

  // Fetch the metadata to be applied from remote file, or return from react-query cache if present
  const {
    data: templateMetadata,
    isLoading,
    isError,
  } = useQuery(
    metadataFileUrl,
    () => fetchTemplateDataQueryFn<string>(metadataFileUrl, {}),
    {
      staleTime,
    }
  );

  const mutation = useMutation(
    (args: MutationFnArgs) => installMetadataMutationFn(args),
    {
      onSuccess: onSuccessCb,
      onError: (error: Error) => {
        if (onErrorCb) {
          onErrorCb(error.message ?? 'Failed to apply metadata');
        }
      },
    }
  );

  const oldMetadata = useAppSelector(state => state.metadata.metadataObject);

  // only do a 'replace_metadata' call if we have the new metadata from the remote url, and current metadata is not null.
  // otherwise `updateMetadata` will just return an empty function. In that case, error callbacks will have info on what went wrong.
  const updateMetadata = useCallback(() => {
    if (templateMetadata && oldMetadata) {
      let templateMetadataJson: HasuraMetadataV3 | undefined;
      if (isJsonString(templateMetadata)) {
        templateMetadataJson = (
          JSON.parse(templateMetadata) as MetadataResponse
        )?.metadata;
      }
      if (templateMetadataJson) {
        const transformedMetadata = transformOldMetadata(
          oldMetadata,
          templateMetadataJson,
          dataSourceName
        );

        mutation.mutate({
          newMetadata: transformedMetadata,
          headers,
        });
      }
    }
    // not adding mutation to dependencies as its a non-memoised function, will trigger this useCallback
    // every time we do a mutation. https://github.com/TanStack/query/issues/1858
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [oldMetadata, templateMetadata, headers, dataSourceName]);

  if (isError) {
    if (onErrorCb) {
      onErrorCb(
        `Failed to fetch metadata from the provided Url: ${metadataFileUrl}`
      );
    }
    return { updateMetadata: undefined };
  }

  if (isLoading) {
    return { updateMetadata: undefined };
  }

  return { updateMetadata };
}
