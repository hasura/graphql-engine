/**
 * Error for valid JSON that the type generator cannot turn into a type. Its
 * message is shown to the user instead of "Invalid JSON".
 */
export class UnsupportedSampleError extends Error {}

const isPlainObject = (value: unknown): value is Record<string, unknown> =>
  typeof value === 'object' && value !== null && !Array.isArray(value);

/**
 * Prepares a JSON sample for `jsonToSchema`, which only handles objects: a
 * top-level array produces invalid GraphQL (a type named `0`), and an empty
 * object produces a type without fields.
 *
 * A non-empty array of objects is reduced to one object holding the fields of
 * all its items, so that the generated type describes an item of the list.
 * Invalid JSON is left as is for `jsonToSchema` to reject.
 */
export const prepareJsonSample = (json: string): string => {
  let value: unknown;
  try {
    value = JSON.parse(json);
  } catch {
    return json;
  }

  const isArray = Array.isArray(value);
  if (Array.isArray(value)) {
    if (value.length === 0) {
      throw new UnsupportedSampleError(
        'Add at least one item to the array to generate a type',
      );
    }
    if (!value.every(isPlainObject)) {
      throw new UnsupportedSampleError(
        'Only arrays of objects can be converted to a type',
      );
    }
    value = Object.assign({}, ...value);
  }

  if (isPlainObject(value) && Object.keys(value).length === 0) {
    throw new UnsupportedSampleError(
      'Add at least one field to the object to generate a type',
    );
  }

  return isArray ? JSON.stringify(value) : json;
};

export const sampleErrorMessage = (e: unknown) =>
  e instanceof UnsupportedSampleError ? e.message : 'Invalid JSON';
