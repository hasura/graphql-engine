// Disabled based on https://github.com/typescript-eslint/typescript-eslint/issues/239
/* eslint-disable no-inner-declarations */
import globals from '../Globals';
import { browserHistory } from 'react-router';
import { APIError } from './error';

interface IApiArgs {
  headers: Record<string, string>;
  url: string;
  method: 'GET' | 'POST' | 'PUT' | 'PATCH' | 'DELETE';
  body?: Record<any, any> | string;
  credentials?: 'include' | 'omit' | 'same-origin';
}

async function fetchApi<T = unknown, V = T>(
  args: IApiArgs,
  dataTransform?: (data: T) => V,
  /**
   * OpenTelemetry offers some new error objects because it uses a new server logic to validate
   * the users input (pease look at the OpenTelemetry errors to gather more info about the errors).
   * At the time of writing, the real reason of the failure would be hidden by the `APIError.fromUnknown`
   * function. At the same time, the OpenTelemetry errors are not a standard yet on the server, so
   * they need a custom management. As/if the new format will be used more, it would be better to
   * remove this custom function to avoid the proliferation of custom usages.
   */
  errorTransform?: (error: unknown) => unknown
): Promise<V> {
  try {
    const { headers, url, method, body, credentials } = args;
    const response = await fetch(url, {
      headers,
      method,
      body: typeof body !== 'string' ? JSON.stringify(body) : body,
      credentials,
    });
    const contentType = response.headers.get('Content-Type');
    const isResponseJson = `${contentType}`.includes('application/json');
    if (response.ok) {
      if (!isResponseJson) {
        return (await response.text()) as unknown as V;
      }
      const data = await response.json();
      if (dataTransform) return dataTransform(data);
      return data;
    }
    if (response.status >= 400) {
      if (!isResponseJson) {
        const errorMessage = await response.text();
        throw errorMessage;
      }
      const errorMessage = await response.json();
      if (errorMessage?.code === 'access-denied') {
        if (window.location.pathname !== `${globals.urlPrefix}/login`) {
          browserHistory.push(`${globals.urlPrefix}/login`);
        }
      }
      throw errorMessage;
    }
    const unknownError = await response.text();
    throw unknownError;
  } catch (error) {
    if (errorTransform) {
      throw errorTransform(error);
    }

    throw APIError.fromUnknown(error);
  }
}

export namespace Api {
  export function get<T = unknown, V = T>(
    args: Omit<IApiArgs, 'body' | 'method'>,
    dataTransform?: (data: T) => V,
    errorTransform?: (error: unknown) => unknown
  ) {
    return fetchApi<T, V>(
      { ...args, method: 'GET' },
      dataTransform,
      errorTransform
    );
  }
  export function post<T = unknown, V = T>(
    args: Omit<IApiArgs, 'method'>,
    dataTransform?: (data: T) => V,
    errorTransform?: (error: unknown) => unknown
  ) {
    return fetchApi<T, V>(
      { ...args, method: 'POST' },
      dataTransform,
      errorTransform
    );
  }
  export function put<T = unknown, V = T>(
    args: Omit<IApiArgs, 'method'>,
    dataTransform?: (data: T) => V,
    errorTransform?: (error: unknown) => unknown
  ) {
    return fetchApi<T, V>(
      { ...args, method: 'PUT' },
      dataTransform,
      errorTransform
    );
  }
  export function del<T = unknown, V = T>(
    args: Omit<IApiArgs, 'method'>,
    dataTransform?: (data: T) => V,
    errorTransform?: (error: unknown) => unknown
  ) {
    return fetchApi<T, V>(
      { ...args, method: 'DELETE' },
      dataTransform,
      errorTransform
    );
  }
  export function base<T = unknown, V = T>(
    args: IApiArgs,
    dataTransform?: (data: T) => V,
    errorTransform?: (error: unknown) => unknown
  ) {
    return fetchApi<T, V>(args, dataTransform, errorTransform);
  }
}
