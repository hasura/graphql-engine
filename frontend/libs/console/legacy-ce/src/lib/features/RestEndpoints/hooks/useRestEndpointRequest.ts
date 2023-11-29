import { RestEndpointEntry } from '../../../metadata/types';
import { Header, Variable } from '../components/RestEndpointDetails';
import {
  getCurrentPageHost,
  getValueWithType,
} from '../../../components/Services/ApiExplorer/Rest/utils';
import { useMutation } from 'react-query';

interface IApiArgs {
  headers: Record<string, string>;
  url: string;
  method: 'GET' | 'POST' | 'PUT' | 'PATCH' | 'DELETE';
  body?: Record<any, any> | string;
}

async function fetchApi<T = unknown, V = T>(args: IApiArgs): Promise<V> {
  const { headers, url, method, body } = args;
  const response = await fetch(url, {
    headers,
    method,
    body: typeof body !== 'string' ? JSON.stringify(body) : body,
  });
  const contentType = response.headers.get('Content-Type');
  const isResponseJson = `${contentType}`.includes('application/json');
  if (response.ok) {
    if (!isResponseJson) {
      return (await response.text()) as unknown as V;
    }
    return response.json();
  }
  throw response.text();
}

export const useRestEndpointRequest = () => {
  const makeRequest = async (options: {
    endpoint: RestEndpointEntry | undefined;
    headers: Header[];
    variables: Variable[];
  }) => {
    const { endpoint, headers, variables } = options;

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

    return fetchApi({
      method: endpoint.methods?.[0],
      url: `${getCurrentPageHost()}/api/rest/${url}`,
      body:
        bodyVariables.length > 0
          ? bodyVariables.reduce((acc, curr) => {
              acc[curr.name] = curr.value;
              return acc;
            }, {} as Record<string, unknown>)
          : undefined,
      headers: selectedHeaders.reduce((acc, curr) => {
        acc[curr.name] = curr.value;
        return acc;
      }, {} as Record<string, string>),
    });
  };

  return useMutation(makeRequest);
};
