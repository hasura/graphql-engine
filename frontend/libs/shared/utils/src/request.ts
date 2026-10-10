import { HttpError } from '@hasura/shared/types';

/**
 * Abstract type of the fetch function.
 */
export type Fetch = (
  input: string | URL,
  init?: RequestInit,
) => Promise<Response>;

/**
 * Thin wrapper around `fetch` that normalizes failures into `HttpError` and
 * `NetworkError` so callers can rely on `catch` instead of checking `response.ok`.
 */
export async function request(
  url: string | URL,
  config?: RequestInit,
): Promise<Response> {
  try {
    const response = await fetch(url, config);

    // fetch() does not throw for non-2xx status codes, so we do it ourselves
    if (!response.ok) {
      const errorText = await response.text();
      const contentType = response.headers.get('content-type');
      if (contentType && contentType?.startsWith('application/json')) {
        let errorPayload: any;
        try {
          errorPayload = JSON.parse(errorText);
        } catch {
          errorPayload = errorText; // Fallback if not JSON
        }

        throw new HttpError(response.status, response.statusText, errorPayload);
      } else {
        throw new HttpError(response.status, response.statusText, errorText);
      }
    }

    return response;
  } catch (error: unknown) {
    if (error instanceof HttpError) {
      throw error; // already normalized above, just propagate
    }

    if (error instanceof TypeError) {
      // browsers throw a generic TypeError for network outages or blocked CORS requests
      throw new HttpError(
        'network-error',
        `Network failure or CORS restriction: ${error.message}`,
      );
    }

    if (error instanceof Error) {
      throw new HttpError('network-error', error.message);
    }

    throw new HttpError(
      'network-error',
      'An unexpected error occurred during execution',
      error,
    );
  }
}

/**
 * Abstract type of the fetch function and decode JSON.
 */
export type FetchJson<T = any> = (
  input: string | URL,
  init?: RequestInit,
) => Promise<T>;

/**
 * Calls `request` and parses the response body as JSON.
 */
export async function requestJson<T>(
  url: string | URL,
  config?: RequestInit,
): Promise<T> {
  const response = await request(url, {
    ...config,
    headers: {
      'Content-Type': 'application/json',
      Accept: 'application/json',
      ...config?.headers,
    },
  });

  try {
    return response.json() as T;
  } catch (error: unknown) {
    const message =
      error instanceof Error ? error.message : JSON.stringify(error);

    throw new Error(
      `An unexpected error occurred during decoding JSON response: ${message}`,
    );
  }
}
