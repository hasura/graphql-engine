import { createControlPlaneClient } from '../ControlPlane';
import endpoints from '../../Endpoints';
import {
  Schema,
  RoleBasedSchema,
  SchemaChange,
  GetSchemaListResponseWithError,
  SchemaRegistryDumpWithSiblingSchema,
  SiblingSchema,
  GetRegistrySchemaResponseWithError,
} from './types';
import {
  getLSItem,
  LS_KEYS,
  removeLSItem,
  setLSItem,
} from '../../utils/localStorage';
import { useCallback, useLayoutEffect, useRef } from 'react';
import moment from 'moment';

export const CapitalizeFirstLetter = (str: string) => {
  return str[0].toUpperCase() + str.slice(1);
};

export const FindIfSubStringExists = (
  originalString: string,
  subString: string
) => {
  return originalString.toLowerCase().includes(subString.toLocaleLowerCase());
};

export const schemaRegsitryLuxDataEndpoint = endpoints.schemaRegistry;

export const schemaRegsitryControlPlaneClient = createControlPlaneClient(
  schemaRegsitryLuxDataEndpoint,
  {}
);

export const schemaListTransformFn = (
  dumps: NonNullable<
    GetSchemaListResponseWithError['data']
  >['schema_registry_dumps']
) => {
  const schemaList: Schema[] = [];

  dumps.forEach((dump: SchemaRegistryDumpWithSiblingSchema) => {
    const roleBasedSchemas: RoleBasedSchema[] = [];

    dump.sibling_schemas.forEach((childSchema: SiblingSchema) => {
      const prevSchemaDiff = childSchema.diff_with_previous_schema;
      let changes: SchemaChange[] | undefined = [];

      if (prevSchemaDiff?.length > 0) {
        changes = [...(prevSchemaDiff[0]?.schema_diff_data || [])];
      } else {
        changes = undefined;
      }

      const roleBasedSchema: RoleBasedSchema = {
        raw: childSchema.schema_sdl,
        role: childSchema.hasura_schema_role,
        hash: childSchema.schema_hash,
        entry_hash: dump.entry_hash,
        id: childSchema.id,
        changes: changes,
      };

      roleBasedSchemas.push(roleBasedSchema);
    });

    const schema: Schema = {
      hash: dump.schema_hash,
      created_at: dump.change_recorded_at,
      id: dump.id,
      entry_hash: dump.entry_hash,
      roleBasedSchemas: roleBasedSchemas,
      tags: dump.schema_tags,
    };

    schemaList.push(schema);
  });
  return schemaList;
};

export const schemaTransformFn = (
  fetchedData: NonNullable<GetRegistrySchemaResponseWithError['data']>
) => {
  const data = fetchedData.schema_registry_dumps[0] || [];

  const roleBasedSchemas: RoleBasedSchema[] = [];

  const prevSchemaDiff = data.diff_with_previous_schema;
  let changes: SchemaChange[] | undefined = [];

  if (prevSchemaDiff?.length > 0) {
    changes = [...(prevSchemaDiff[0]?.schema_diff_data || [])];
  } else {
    changes = undefined;
  }

  const roleBasedSchema: RoleBasedSchema = {
    id: data.id,
    hash: data.schema_hash,
    raw: data.schema_sdl,
    role: data.hasura_schema_role,
    entry_hash: data.entry_hash,
    changes: changes,
  };

  roleBasedSchemas.push(roleBasedSchema);

  const schema: Schema = {
    hash: data.schema_hash,
    entry_hash: data.entry_hash,
    created_at: data.change_recorded_at,
    id: data.id,
    roleBasedSchemas: roleBasedSchemas,
    tags: data.schema_tags,
  };

  return schema;
};

export const getPublishTime = (isoStringTs: string) => {
  const published = moment(isoStringTs);
  return published.format('DD/MM/YYYY HH:mm:ss');
};

export const SLACK_CALLBACK_SEARCH = LS_KEYS.slackCallbackSearch;

export const persistSlackCallbackSearch = (value: string) => {
  setLSItem(SLACK_CALLBACK_SEARCH, value);
};

export const getPersistedSlackCallbackSearch = () => {
  return getLSItem(SLACK_CALLBACK_SEARCH);
};

export const clearPersistedSlackCallbackSearch = () => {
  removeLSItem(SLACK_CALLBACK_SEARCH);
};

export function useIsUnmounted() {
  const rIsUnmounted = useRef<'mounting' | 'mounted' | 'unmounted'>('mounting');

  useLayoutEffect(() => {
    rIsUnmounted.current = 'mounted';
    return () => {
      rIsUnmounted.current = 'unmounted';
    };
  }, []);

  return useCallback(() => rIsUnmounted.current !== 'mounted', []);
}

export const generateRandomString = (stringLength = 16) => {
  const allChars =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  let str = '';

  for (let i = 0; i < stringLength; i++) {
    const randomNum = Math.floor(Math.random() * allChars.length);
    str += allChars.charAt(randomNum);
  }
  return str;
};

export const hexToRGB = (hex: string, alpha: number) => {
  const r = parseInt(hex.slice(1, 3), 16),
    g = parseInt(hex.slice(3, 5), 16),
    b = parseInt(hex.slice(5, 7), 16);

  if (alpha) {
    return 'rgba(' + r + ', ' + g + ', ' + b + ', ' + alpha + ')';
  } else {
    return 'rgb(' + r + ', ' + g + ', ' + b + ')';
  }
};
