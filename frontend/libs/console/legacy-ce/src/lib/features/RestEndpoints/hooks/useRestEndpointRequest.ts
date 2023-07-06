import { RestEndpointEntry } from '../../../metadata/types';
import { Header, Variable } from '../components/RestEndpointDetails';
import { Api } from '../../../hooks/apiUtils';
import {
  getCurrentPageHost,
  getValueWithType,
} from '../../../components/Services/ApiExplorer/Rest/utils';
import { useQuery } from 'react-query';

type QueryKey = [
  string,
  {
    endpoint: RestEndpointEntry | undefined;
    headers: Header[];
    variables: Variable[];
  }
];

export const useRestEndpointRequest = (
  endpoint: RestEndpointEntry | undefined,
  headers: Header[],
  variables: Variable[]
) => {
  const makeRequest = ({ queryKey }: { queryKey: QueryKey }) => {
    const [, { endpoint, headers, variables }] = queryKey;

    const selectedHeaders = headers
      .filter(h => !!h.name && h.selected)
      .map(h => ({ name: h.name, value: h.value }));

    const processedVariable = variables.map(v => ({
      name: v.name,
      value: getValueWithType(v),
    }));

    const bodyVariables = [];

    if (!endpoint) {
      return;
    }

    let url = endpoint.url;

    for (const variable of processedVariable) {
      if (url.match(`/:${variable.name}`)) {
        url = url.replace(`/:${variable.name}`, `/${variable.value}`);
      } else {
        bodyVariables.push(variable);
      }
    }

    return Api.base({
      method: endpoint.methods?.[0],
      url: `${getCurrentPageHost()}/api/rest/${url}`,
      body: bodyVariables.reduce((acc, curr) => {
        acc[curr.name] = curr.value;
        return acc;
      }, {} as Record<string, unknown>),
      headers: selectedHeaders.reduce((acc, curr) => {
        acc[curr.name] = curr.value;
        return acc;
      }, {} as Record<string, string>),
    });
  };

  return useQuery({
    queryKey: [
      'rest-endpoint-request ',
      { endpoint, headers, variables },
    ] as QueryKey,
    queryFn: makeRequest,
    enabled: false,
    retry: 1,
  });
};
