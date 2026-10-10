import pickBy from 'lodash/pickBy';
import { Schema } from './schema';
import type {
  RemoteSchema,
  RemoteSchemaCustomization,
  RemoteSchemaDefinition,
} from '@hasura/shared/types';
import {
  parseHeaderConfigs,
  transformHeaderConfigs,
} from '@hasura/shared/utils';

/**
 * @param existingCustomization the remote schema's current customization when
 * editing. The form only edits part of it (namespace, type prefix/suffix and
 * root field prefix/suffix), so everything else — `type_names.mapping`,
 * `field_names` of other types, and each entry's `mapping` — is carried over
 * instead of being wiped on save.
 */
export const transformFormData = (
  values: Schema,
  existingCustomization?: RemoteSchemaCustomization,
): RemoteSchema => {
  const {
    name,
    url,
    headers,
    use_introspection_headers,
    introspection_headers,
    forward_client_headers,
    comment,
    timeout_seconds,
    customization: {
      root_fields_namespace,
      type_prefix,
      type_suffix,
      query_root,
      mutation_root,
    },
  } = values;

  const customization: RemoteSchemaCustomization = {};

  /* if root field namespace is present */
  if (root_fields_namespace && root_fields_namespace !== '')
    customization.root_fields_namespace = root_fields_namespace;

  /* type prefix & suffix from the form, keeping any existing type mapping */
  const typeNames = pickBy(
    {
      ...existingCustomization?.type_names,
      prefix: type_prefix,
      suffix: type_suffix,
    },
    (value) =>
      typeof value === 'string' ? value.length > 0 : value !== undefined,
  );
  if (Object.keys(typeNames).length > 0) customization.type_names = typeNames;

  /**
   * `field_names` is a list keyed by `parent_type`, so the Query and Mutation
   * root customizations must be collected together. Assigning them separately
   * would let the Mutation entry overwrite the Query one whenever both are set.
   * Existing entries (for any type) are kept, and a root entry from the form
   * updates the prefix/suffix of the entry with the same `parent_type`.
   */
  const fieldNames: NonNullable<RemoteSchemaCustomization['field_names']> = [
    ...(existingCustomization?.field_names ?? []),
  ];

  const upsertFieldNames = (root: Schema['customization']['query_root']) => {
    if (!root.parent_type || !(root.prefix || root.suffix)) return;

    // Only what was entered: root entries aren't loaded into the form, so an
    // empty input must not erase an existing prefix/suffix.
    const entry = {
      parent_type: root.parent_type,
      ...(root.prefix ? { prefix: root.prefix } : {}),
      ...(root.suffix ? { suffix: root.suffix } : {}),
    };
    const index = fieldNames.findIndex(
      (f) => f.parent_type === root.parent_type,
    );
    if (index === -1) {
      fieldNames.push(entry);
    } else {
      fieldNames[index] = { ...fieldNames[index], ...entry };
    }
  };

  /* if Query root customization is present */
  upsertFieldNames(query_root);

  /* if Mutation root customization is present */
  upsertFieldNames(mutation_root);

  if (fieldNames.length > 0) customization.field_names = fieldNames;

  const definition: RemoteSchemaDefinition =
    url.type === 'from_env'
      ? {
          url_from_env: url.value,
        }
      : {
          url: url.value,
        };
  definition.forward_client_headers = forward_client_headers;
  definition.customization = customization;
  definition.headers = transformHeaderConfigs(headers);
  definition.timeout_seconds = timeout_seconds || 60;

  /**
   * Only serialize `introspection_headers` when the user has explicitly opted
   * in. This preserves the three metadata states:
   * - opt-out  -> property omitted, introspection inherits request `headers`;
   * - opt-in, empty list -> `introspection_headers: []` (send no headers);
   * - opt-in, with values -> the mapped list.
   * The runtime request `headers` are never affected by this list.
   */
  if (use_introspection_headers) {
    definition.introspection_headers = transformHeaderConfigs(
      introspection_headers,
    );
  }

  return { name, comment, definition };
};

export const createRemoteSchemaFormValues = (
  defaultValues?: RemoteSchema,
): Schema => {
  const definition = defaultValues?.definition;

  const url: Schema['url'] =
    definition && 'url_from_env' in definition
      ? { value: definition.url_from_env, type: 'from_env' }
      : {
          value: definition && 'url' in definition ? definition.url : '',
          type: 'from_url',
        };

  /**
   * Recover the three introspection-header states from existing metadata so the
   * edit form round-trips them:
   * - property absent      -> disabled (introspection inherits request headers);
   * - explicit empty array -> enabled with no headers;
   * - non-empty array      -> enabled with the parsed headers.
   */
  const useIntrospectionHeaders = Array.isArray(
    definition?.introspection_headers,
  );

  return {
    name: defaultValues?.name ?? '',
    url,
    headers: parseHeaderConfigs(definition?.headers),
    use_introspection_headers: useIntrospectionHeaders,
    introspection_headers: parseHeaderConfigs(
      definition?.introspection_headers,
    ),
    forward_client_headers: definition?.forward_client_headers ?? false,
    timeout_seconds: definition?.timeout_seconds ?? 60,
    comment: defaultValues?.comment ?? '',
    customization: {
      root_fields_namespace:
        definition?.customization?.root_fields_namespace ?? '',
      type_prefix: definition?.customization?.type_names?.prefix ?? '',
      type_suffix: definition?.customization?.type_names?.suffix ?? '',
      // Root field entries can't be told apart from other types' `field_names`
      // without introspection, so they stay empty here; `transformFormData`
      // keeps the existing entries.

      query_root: {
        parent_type: '',
        prefix: '',
        suffix: '',
      },
      mutation_root: {
        parent_type: '',
        prefix: '',
        suffix: '',
      },
    },
  };
};
