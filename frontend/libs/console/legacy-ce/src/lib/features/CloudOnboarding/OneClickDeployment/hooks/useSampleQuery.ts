import { useQuery } from '@tanstack/react-query';
import { requestJson } from '@hasura/shared/utils';

export function fetchSampleQuery(dataUrl: string) {
  return requestJson(dataUrl, {
    method: 'GET',
    headers: {},
  });
}

export const useSampleQuery = (sampleQueriesPath: string) => {
  const { data: sampleQueriesData } = useQuery({
    queryKey: [sampleQueriesPath],
    queryFn: () => fetchSampleQuery(sampleQueriesPath),
    staleTime: 300000,
  });

  if (typeof sampleQueriesData === 'string') {
    return sampleQueriesData;
  }

  return '';
};
