import { HeaderConfig } from '../header';
import { RemoteRelationship } from '../source';

type RemoteSchemaName = string;

export type RemoteSchemaPermission = {
  remote_schema_name: RemoteSchemaName;
  definition: { schema: string };
  role: string;
  comment?: string;
};

export type RemoteSchemaCustomization = {
  root_fields_namespace?: string;
  type_names?: {
    prefix?: string;
    suffix?: string;
    mapping?: Record<string, string>;
  };
  field_names?: {
    parent_type: string;
    prefix?: string;
    suffix?: string;
    mapping?: Record<string, string>;
  }[];
};

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/syntax-defs.html#remoteschemadef
 */
interface BaseDefinition {
  headers?: HeaderConfig[];
  /**
   * Headers used only when fetching (introspecting) the Remote Schema.
   *
   * - omitted: introspection reuses the ordinary request `headers`.
   * - empty array: introspection sends no configured headers.
   * - non-empty: introspection uses only these headers.
   */
  introspection_headers?: HeaderConfig[];
  forward_client_headers?: boolean;
  timeout_seconds?: number;
  customization?: RemoteSchemaCustomization;
}

export interface RemoteSchemaDefinitionWithUrl extends BaseDefinition {
  url: string;
}

export interface RemoteSchemaDefinitionWithEnv extends BaseDefinition {
  url_from_env: string;
}

export type RemoteSchemaDefinition =
  RemoteSchemaDefinitionWithUrl | RemoteSchemaDefinitionWithEnv;

export type RemoteSchema = {
  name: RemoteSchemaName;
  definition: RemoteSchemaDefinition;
  comment?: string;
  permissions?: RemoteSchemaPermission[];
  remote_relationships?: RemoteSchemaRemoteRelationship[];
};

export type RemoteSchemaRemoteRelationship = {
  relationships: RemoteRelationship[];
  type_name: string;
};
