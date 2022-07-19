import { useQuery } from 'react-query';
import { getTreeData } from '../utils';

export const useTreeData = <TreeGDCSource>() => {
  return useQuery({
    queryKey: 'treeview',
    queryFn: async () => {
      return getTreeData<TreeGDCSource>();
    },
  });
};
