// Disabled based on https://github.com/typescript-eslint/typescript-eslint/issues/239
/* eslint-disable no-inner-declarations */
import globals from '@/Globals';
import { browserHistory } from 'react-router';

interface IApiArgs {
  headers: Record<string, string>;
  url: string;
  method: 'GET' | 'POST' | 'PUT' | 'PATCH' | 'DELETE';
  body?: Record<any, any>;
}

// Adapted from https://github.com/hasura/graphql-engine-mono/blob/88257687a2c989369b62115c238aa318ea9780ca/console/src/components/Services/Common/Notification.tsx#L80-L129
// FIXME: use unknown here
function processError(error: any): string {
  if (typeof error === 'string') {
    return error;
  } else if (
    error?.message?.error === 'postgres query error' ||
    error.message?.error === 'query execution failed'
  ) {
    if (error.message.internal) {
      return `${error.message.code}: ${error.message.internal.error?.message}`;
    }
    return `${error.code}: ${error.message.error}`;
  } else if ('info' in error) {
    return error.info;
  } else if ('message' in error) {
    if (error.code) {
      if (error.message.error) {
        return error.message.error.message;
      }
      return error.message;
    } else if (typeof error?.message === 'string') {
      return error.message;
    } else if ('code' in error?.message) {
      return error.message.code;
    }
    return error.code;
  } else if (error.internal?.error) {
    return `${error.internal.error.message}.
      ${error.internal.error.description}`;
  } else if ('custom' in error) {
    return error.custom;
  } else if ('code' in error && 'error' in error && 'path' in error) {
    return error.error;
  }
  return JSON.stringify(error ?? 'request failed');
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
    throw new Error(processError(error));
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
  // Http request with method set to DELETE
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
