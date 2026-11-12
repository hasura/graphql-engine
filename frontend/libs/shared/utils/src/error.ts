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

const DEPENDENT_OBJECTS_ERROR_PREFIX =
  'cannot drop due to the following dependent objects: ';

/**
 * The server lists the objects that prevent a drop on a single line,
 * separated by commas, which is hard to read when there are many of them.
 * Returns the message with one object per line, or null for other messages.
 */
export const formatDependentObjectsError = (message: unknown) => {
  const objects = splitDependentObjects(message);
  if (!objects) {
    return null;
  }

  return [
    DEPENDENT_OBJECTS_ERROR_PREFIX.trimEnd(),
    ...objects.map((object) => `- ${object}`),
  ].join('\n');
};

const splitDependentObjects = (message: unknown) => {
  if (
    typeof message !== 'string' ||
    !message.startsWith(DEPENDENT_OBJECTS_ERROR_PREFIX)
  ) {
    return null;
  }

  // A name may itself contain ', ', so only split where the next part starts
  // with an object kind.
  return message
    .slice(DEPENDENT_OBJECTS_ERROR_PREFIX.length)
    .split(', ')
    .reduce<string[]>((objects, part) => {
      if (objects.length > 0 && !DEPENDENT_OBJECT_KIND.test(part)) {
        objects[objects.length - 1] += `, ${part}`;
      } else {
        objects.push(part);
      }
      return objects;
    }, []);
};

// The kinds of objects the server reports, see `reportSchemaObj` in
// server/src-lib/Hasura/RQL/Types/SchemaCacheTypes.hs
const DEPENDENT_OBJECT_KIND =
  /^(source|table|function|native query|column|stored procedure|logical model|logical model column|inner logical model|permission|relationship|constraint|event-trigger|computed field|remote relationship|remote schema|remote schema permission|remote_relationship|role) /;

/**
 * Returns the objects that prevent a drop when the server rejected it with a
 * `dependency-error`, or null for any other error.
 */
export const getDependentObjects = (error: unknown): string[] | null => {
  const data =
    error instanceof HttpError
      ? error.data
      : error instanceof Error
        ? { error: error.message }
        : error;

  if (!data || typeof data !== 'object' || !('error' in data)) {
    return null;
  }

  return splitDependentObjects((data as { error?: unknown }).error);
};

export type DependencyError = {
  // The object that failed, e.g. "table Album in source default". Only set
  // for bulk operations.
  name?: string;
  // The objects that depend on it, or null when it failed for another reason.
  objects: string[] | null;
  message: string;
};

/**
 * Returns what prevented a drop because of dependent objects, for both a
 * single `dependency-error` and bulk operations such as `pg_untrack_tables`,
 * which report each failed object in `internal` when they all failed, or in
 * the `warnings` of a successful response when only some failed. Returns null
 * when none of the failures are caused by dependent objects.
 */
export const getDependencyErrors = (
  error: unknown,
): DependencyError[] | null => {
  const objects = getDependentObjects(error);
  if (objects) {
    return [{ objects, message: String(getErrorMessage(error)) }];
  }

  const data = (error instanceof HttpError ? error.data : error) as
    { internal?: unknown; warnings?: unknown } | undefined;
  const failures = data?.internal ?? data?.warnings;
  if (!Array.isArray(failures)) {
    return null;
  }

  const errors = failures.map(
    (warning: { name?: unknown; message?: unknown }) => ({
      name: typeof warning?.name === 'string' ? warning.name : undefined,
      objects: splitDependentObjects(warning?.message),
      message: String(warning?.message ?? ''),
    }),
  );

  return errors.some((e) => e.objects) ? errors : null;
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
    return (
      formatDependentObjectsError((error.data as { error?: unknown })?.error) ??
      (error.data ? error.data : error.message)
    );
  }

  if (error instanceof Error) {
    return formatDependentObjectsError(error.message) ?? error.message;
  }

  if (typeof error !== 'object') {
    return error || message;
  }

  return (
    formatDependentObjectsError((error as { error?: unknown }).error) ?? error
  );
};
