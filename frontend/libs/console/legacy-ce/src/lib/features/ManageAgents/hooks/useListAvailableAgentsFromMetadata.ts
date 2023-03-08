import { exportMetadata } from '../../DataSource';
import { useHttpClient } from '../../Network';
import { useQuery } from 'react-query';
import { DcAgent } from '../types';

export const useListAvailableAgentsFromMetadata = () => {
  const httpClient = useHttpClient();
  return useQuery({
    queryKey: ['agent_list'],
    queryFn: async () => {
      const { metadata } = await exportMetadata({ httpClient });

      if (!metadata)
        throw Error(
          'useListAvailableAgentFromMetadata: could not fetch metadata'
        );

      const backend_configs = metadata.backend_configs;

      if (!backend_configs) return [];

      const values = Object.entries(backend_configs.dataconnector).map<DcAgent>(
        item => {
          const [dcAgentName, definition] = item;

          return {
            name: dcAgentName,
            url: definition.uri,
          };
        }
      );

      // sort the values by ascending order of name. It's visually easier to read the items.
      const result = values.sort((a, b) =>
        a.name > b.name ? 1 : b.name > a.name ? -1 : 0
      );

      return result;
    },
    refetchOnWindowFocus: false,
  });
};
