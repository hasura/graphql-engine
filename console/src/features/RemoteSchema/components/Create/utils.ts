import pickBy from 'lodash.pickby';
import { Schema } from './schema';

export const transformFormData = (values: Schema) => {
  const {
    name,
    url,
    headers,
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

  const customization: Record<string, any> = {};

  /* if root field namespace is present */
  if (root_fields_namespace)
    customization.root_fields_namespace = root_fields_namespace;

  /* if type prefix & suffix are present */
  if (type_prefix || type_suffix)
    customization.type_names = pickBy(
      {
        prefix: type_prefix,
        suffix: type_suffix,
      },
      value => value.length > 0
    );

  /* if Query root customization is present */
  if (query_root.parent_type && (query_root.prefix || query_root.suffix)) {
    customization.field_names = [
      ...(customization.field_names ?? []),
      pickBy(
        {
          parent_type: query_root.parent_type,
          prefix: query_root.prefix,
          suffix: query_root.suffix,
        },
        value => value.length > 0
      ),
    ];
  }

  /* if Mutation root customization is present */
  if (
    mutation_root.parent_type &&
    (mutation_root.prefix || mutation_root.suffix)
  ) {
    customization.field_names = [
      ...(customization.field_names ?? []),
      pickBy(
        {
          parent_type: mutation_root.parent_type,
          prefix: mutation_root.prefix,
          suffix: mutation_root.suffix,
        },
        value => value.length > 0
      ),
    ];
  }

  const definition: Record<string, any> = {
    url,
    forward_client_headers,
    comment,
    headers: headers.map(header => {
      if (header.type === 'from_env')
        return { name: header.name, value_from_env: header.value };
      return { name: header.name, value: header.value };
    }),
    timeout_seconds: timeout_seconds ? parseInt(timeout_seconds, 10) : 60,
    customization,
  };

  return { name, definition };
};
