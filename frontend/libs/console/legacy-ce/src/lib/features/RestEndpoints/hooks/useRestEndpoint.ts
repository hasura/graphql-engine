import { useMetadata } from '../../MetadataAPI';

export const useRestEndpoint = (name: string) => {
  const { data: metadata } = useMetadata();

  const endpoint = metadata?.metadata?.rest_endpoints?.find(
    endpoint => endpoint.name === name
  );

  const queryCollection = metadata?.metadata?.query_collections?.find(
    collection =>
      collection.name === endpoint?.definition?.query?.collection_name
  );

  const query = queryCollection?.definition?.queries?.find(
    query => query.name === endpoint?.definition?.query?.query_name
  );

  if (!endpoint || !query) {
    return null;
  }

  return {
    endpoint,
    query,
  };
};
