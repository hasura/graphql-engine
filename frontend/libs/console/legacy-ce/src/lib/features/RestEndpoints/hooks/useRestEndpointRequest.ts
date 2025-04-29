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
    const queryParams = [];

    if (!endpoint) {
      return;
    }

    let url = endpoint.url;
    const method = endpoint.methods?.[0];

    for (const variable of processedVariable) {
      if (url.match(`/:${variable.name}`)) {
        url = url.replace(`/:${variable.name}`, `/${variable.value}`);
      } else if (method === 'GET') {
        // For GET requests, add variables as query parameters
        // Only handle scalar types (string, number, boolean) for GET requests
        // Check if it's a scalar type (not an array or object)
        const isScalar =
          typeof variable.value !== 'object' || variable.value === null;

        if (isScalar) {
          // Use the value directly as processed by getValueWithType
          // This ensures consistent handling between GET and other methods
          queryParams.push(
            `${variable.name}=${encodeURIComponent(String(variable.value))}`
          );
        } else {
          // For non-scalar types (arrays, objects), switch to using the body
          // This will make the request fail with a clear error message
          bodyVariables.push(variable);
        }
      } else {
        bodyVariables.push(variable);
      }
    }

    // Append query parameters to the URL if using GET
    let fullUrl = `${getCurrentPageHost()}/api/rest/${url}`;
    if (method === 'GET' && queryParams.length > 0) {
      fullUrl += `?${queryParams.join('&')}`;
    }

    return fetchApi({
      method: method,
      url: fullUrl,
      body:
        bodyVariables.length > 0 && method !== 'GET'
          ? bodyVariables.reduce((acc, curr) => {
              // Use the value directly as processed by getValueWithType
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
