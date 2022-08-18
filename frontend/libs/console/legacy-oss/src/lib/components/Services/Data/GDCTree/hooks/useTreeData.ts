import { useHttpClient } from '@/features/Network';
import { useQuery } from 'react-query';
import { getTreeData } from '../utils';

export const useTreeData = ({
  headers,
}: {
  headers: Record<string, string>;
}) => {
  const httpClient = useHttpClient({ headers });

  return useQuery({
    queryKey: 'treeview',
    queryFn: async () => {
      return getTreeData({ httpClient });
    },
  });
};
