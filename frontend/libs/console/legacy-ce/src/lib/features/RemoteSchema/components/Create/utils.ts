import pickBy from 'lodash/pickBy';
import { Dictionary } from 'ts-essentials';
import { Schema } from './schema';

interface customizationType {
  root_fields_namespace?: string;
  field_names?: {
    parent_type?: string;
    prefix?: string;
    suffix?: string;
  }[];
  type_names?: Dictionary<string>;
}

interface Definition {
  timeout_seconds: number;
  url_from_env?: string;
  url?: string;
  forward_client_headers: boolean;
  customization: customizationType;
  headers: (
    | { name: string; value_from_env: string }
    | { name: string; value: string }
  )[];
}

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

  const customization: customizationType = {};

  /* if root field namespace is present */
  if (root_fields_namespace && root_fields_namespace !== '')
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

  const definition: Definition = {
    [url.type === 'from_env' ? 'url_from_env' : 'url']: url.value,
    forward_client_headers,
    headers: headers.map(header => {
      if (header.type === 'from_env')
        return { name: header.name, value_from_env: header.value };
      return { name: header.name, value: header.value };
    }),
    timeout_seconds: timeout_seconds ? parseInt(timeout_seconds, 10) : 60,
    customization,
  };

  return { name, comment, definition };
};
