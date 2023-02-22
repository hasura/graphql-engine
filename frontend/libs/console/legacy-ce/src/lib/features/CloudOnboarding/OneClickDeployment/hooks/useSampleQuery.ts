import { useQuery } from 'react-query';
import { Api } from '../../../../hooks/apiUtils';

export function fetchSampleQuery(dataUrl: string) {
  return Api.get({
    url: dataUrl,
    headers: {},
  });
}

export const useSampleQuery = (sampleQueriesPath: string) => {
  const { data: sampleQueriesData } = useQuery({
    queryKey: sampleQueriesPath,
    queryFn: () => fetchSampleQuery(sampleQueriesPath),
    staleTime: 300000,
  });

  if (typeof sampleQueriesData === 'string') {
    return sampleQueriesData;
  }

  return '';
};
