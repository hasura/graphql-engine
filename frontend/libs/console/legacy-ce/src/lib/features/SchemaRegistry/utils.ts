import { createControlPlaneClient } from '../ControlPlane';
import endpoints from '../../Endpoints';
import {
  Schema,
  RoleBasedSchema,
  SchemaChange,
  GetRegistrySchemaResponseWithError,
  Role,
  SchemaChangeCard,
  SchemaChangeListDumpWithSiblingSchema,
  SchemaChangeListSiblingSchema,
} from './types';
import { getLSItem, removeLSItem, setLSItem } from '@hasura/shared/utils';
import { format } from 'date-fns';
import { LS_KEYS } from '@hasura/shared/types';

export const CapitalizeFirstLetter = (str: string) => {
  if (str === '') {
    return '';
  }
  return str[0].toUpperCase() + str.slice(1);
};

export const findIfSubStringExists = (
  originalString: string,
  subString: string,
) => {
  return originalString.toLowerCase().includes(subString.toLocaleLowerCase());
};

export const schemaRegsitryLuxDataEndpoint = endpoints.schemaRegistry;

export const schemaRegsitryControlPlaneClient = createControlPlaneClient(
  schemaRegsitryLuxDataEndpoint,
  {},
);
export const getSearchParam = (search: string, param: string) => {
  try {
    const params = new URLSearchParams(search);
    return params.get(param) || null;
  } catch {
    return null;
  }
};

export const setSearchParam = (pageNumber: number) => {
  const params = new URLSearchParams(window.location.search);
  params.set('page', pageNumber.toString());
  const newUrl = `${window.location.pathname}?${params.toString()}`;
  window.history.pushState({}, '', newUrl);
};

export const schemaChangeListTransformFn = (
  dumps: SchemaChangeListDumpWithSiblingSchema[],
) => {
  const schemaList: SchemaChangeCard[] = [];
  dumps.forEach((dump: SchemaChangeListDumpWithSiblingSchema) => {
    const schemaRoles: Role[] = [];
    dump.sibling_schemas.forEach(
      (childSchema: SchemaChangeListSiblingSchema) => {
        const schemaRole: Role = {
          id: childSchema.id,
          role: childSchema.hasura_schema_role,
        };
        schemaRoles.push(schemaRole);
      },
    );
    const schema: SchemaChangeCard = {
      hash: dump.schema_hash,
      created_at: dump.change_recorded_at,
      id: dump.id,
      entry_hash: dump.entry_hash,
      roles: schemaRoles,
      tags: dump.schema_tags,
    };

    schemaList.push(schema);
  });
  return schemaList;
};

export const schemaTransformFn = (
  fetchedData: NonNullable<GetRegistrySchemaResponseWithError['data']>,
): Schema | null => {
  const data =
    fetchedData.schema_registry_dumps[0] ||
    fetchedData.schema_registry_dumps_v2[0];
  if (!data) {
    return null;
  }

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
  return format(new Date(isoStringTs), 'dd/MM/yyyy HH:mm:ss');
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
