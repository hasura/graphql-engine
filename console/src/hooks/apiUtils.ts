// Disabled based on https://github.com/typescript-eslint/typescript-eslint/issues/239
/* eslint-disable no-inner-declarations */
import globals from '@/Globals';
import { browserHistory } from 'react-router';
import { APIError } from './error';

interface IApiArgs {
  headers: Record<string, string>;
  url: string;
  method: 'GET' | 'POST' | 'PUT' | 'PATCH' | 'DELETE';
  body?: Record<any, any>;
}

async function fetchApi<T = unknown, V = T>(
  args: IApiArgs,
  dataTransform?: (data: T) => V
): Promise<V> {
  try {
    const { headers, url, method, body } = args;
    const response = await fetch(url, {
      headers,
      method,
      body: JSON.stringify(body),
    });
    const contentType = response.headers.get('Content-Type');
    const isResponseJson = `${contentType}`.includes('application/json');
    if (response.ok) {
      if (!isResponseJson) {
        return ((await response.text()) as unknown) as V;
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
    throw APIError.fromUnknown(error);
  }
}

export namespace Api {
  export function get<T = unknown, V = T>(
    args: Omit<IApiArgs, 'body' | 'method'>,
    dataTransform?: (data: T) => V
  ) {
    return fetchApi<T, V>({ ...args, method: 'GET' }, dataTransform);
  }
  export function post<T = unknown, V = T>(
    args: Omit<IApiArgs, 'method'>,
    dataTransform?: (data: T) => V
  ) {
    return fetchApi<T, V>({ ...args, method: 'POST' }, dataTransform);
  }
  export function put<T = unknown, V = T>(
    args: Omit<IApiArgs, 'method'>,
    dataTransform?: (data: T) => V
  ) {
    return fetchApi<T, V>({ ...args, method: 'PUT' }, dataTransform);
  }
  export function del<T = unknown, V = T>(
    args: Omit<IApiArgs, 'method'>,
    dataTransform?: (data: T) => V
  ) {
    return fetchApi<T, V>({ ...args, method: 'DELETE' }, dataTransform);
  }
  export function base<T = unknown, V = T>(
    args: IApiArgs,
    dataTransform?: (data: T) => V
  ) {
    return fetchApi<T, V>(args, dataTransform);
  }
}
