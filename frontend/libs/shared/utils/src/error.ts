import { HttpError } from '@hasura/shared/types';

export const isConsoleError = (x: unknown): x is Error => {
  return x instanceof Error;
};

/**
 * Get the underlying message of the error.
 * @param err an error instance.
 * @param message the fallback error message.
 * @returns error message.
 */
export const getErrorMessage = (
  error: unknown,
  message: string | null = 'Unknown error',
): string => {
  if (!error) {
    return message ?? '';
  }

  if (typeof error === 'string') {
    return error;
  }

  if (error instanceof HttpError) {
    return error.data
      ? JSON.stringify(error.data, undefined, 4)
      : error.message;
  }

  if (error instanceof Error) {
    return error.message;
  }

  if (typeof error !== 'object') {
    return message ?? '';
  }

  if ('info' in error && error.info) {
    return typeof error.info === 'string'
      ? error.info
      : JSON.stringify(error.info);
  }

  if ('message' in error && error.message) {
    if (
      typeof error.message === 'object' &&
      'error' in error.message &&
      (error.message?.error === 'postgres query error' ||
        error.message?.error === 'query execution failed')
    ) {
      if ('internal' in error.message && error.message.internal) {
        return `${(error.message as Record<string, any>)['code'] ?? ''}: ${
          (error.message.internal as Record<string, any>)['error']?.message
        }`;
      }

      return `${(error as Record<string, any>)['code']}: ${error.message.error}`;
    }

    if ('code' in error && error.code) {
      if (
        typeof error.message == 'object' &&
        error.message &&
        'error' in error.message &&
        error.message.error &&
        typeof error.message.error === 'object'
      ) {
        return (error.message.error as Record<string, string>)['message'] ?? '';
      }

      if (typeof error.message === 'string') {
        return error.message;
      }
    }

    if (typeof error.message === 'string') {
      return error.message;
    }

    if (
      typeof error.message === 'object' &&
      'code' in error.message &&
      error.message.code
    ) {
      return `${error.message.code} : ${message}`;
    }

    if ('code' in error && error.code) {
      return typeof error.code === 'string'
        ? error.code
        : typeof error.code === 'number'
          ? String(error.code)
          : '';
    }
  }

  if (
    'internal' in error &&
    error.internal &&
    typeof error.internal === 'object' &&
    'error' in error.internal &&
    error.internal?.error
  ) {
    const internalError = error.internal.error as Record<string, string>;
    return `${internalError['message']}.${internalError['description']}`;
  }

  if ('custom' in error && error.custom) {
    return String(error.custom);
  }

  if ('error' in error && error.error) {
    // Data API error
    if (typeof error.error === 'object') {
      return (error.error as Record<string, string>)['message'] ?? '';
    }

    return String(error.error);
  }

  if ('callToAction' in error && message) {
    return message;
  }

  const jsonError = JSON.stringify(error);

  return jsonError !== '{}' ? jsonError : (message ?? '');
};

/**
 * Get the underlying content of the error.
 * @param err an error instance.
 * @param message the fallback error message.
 * @returns error message.
 */
export const getErrorContent = (
  error: unknown,
  message: string | null = 'Unknown error',
): unknown => {
  if (!error) {
    return message ?? '';
  }

  if (typeof error === 'string') {
    return error;
  }

  if (error instanceof HttpError) {
    return error.data ? error.data : error.message;
  }

  if (error instanceof Error) {
    return error.message;
  }

  if (typeof error !== 'object') {
    return error || message;
  }

  return error;
};
