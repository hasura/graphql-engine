import { useHttpClient } from '@/features/Network';
import { useQuery } from 'react-query';
import { getTreeData } from '../utils';

export const useTreeData = () => {
  const httpClient = useHttpClient();

  return useQuery({
    queryKey: 'treeview',
    queryFn: async () => {
      return getTreeData({ httpClient });
    },
  });
};
