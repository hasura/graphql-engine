import { useQuery } from '@tanstack/react-query';
import { DcAgent } from '../types';
import { useMetadataHelpers } from '@hasura/metadata/api';

export const useListAvailableAgentsFromMetadata = () => {
  const { fetchMetadata } = useMetadataHelpers();

  return useQuery({
    queryKey: ['agent_list'],
    queryFn: async () => {
      const data = await fetchMetadata();

      const backend_configs = data.metadata.backend_configs;

      if (!backend_configs) return [];

      const values = Object.entries(backend_configs.dataconnector).map<DcAgent>(
        (item) => {
          const [dcAgentName, definition] = item;

          return {
            name: dcAgentName,
            uri: definition.uri,
          };
        },
      );

      // sort the values by ascending order of name. It's visually easier to read the items.
      const result = values.sort((a, b) =>
        a.name > b.name ? 1 : b.name > a.name ? -1 : 0,
      );

      return result;
    },
    refetchOnWindowFocus: false,
  });
};
