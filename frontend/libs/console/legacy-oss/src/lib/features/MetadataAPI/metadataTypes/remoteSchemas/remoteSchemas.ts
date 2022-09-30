import { RemoteField, Table } from '../source';

type RemoteSchemaName = string;

export type RemoteSchemaPermission = {
  remote_schema_name: RemoteSchemaName;
  definition: { schema: string };
  role: string;
  comment?: string;
};

export type ServerHeader =
  | {
      name: string;
      value?: string;
    }
  | {
      name: string;
      value_from_env?: string;
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

interface BaseDefinition {
  url?: string;
  url_from_env?: string;
  headers?: ServerHeader[];
  forward_client_headers?: boolean;
  timeout_seconds?: number;
  customization?: RemoteSchemaCustomization;
}

interface RemoteSchemaDefinitionWithUrl extends BaseDefinition {
  url: string;
}

interface RemoteSchemaDefinitionWithEnv extends BaseDefinition {
  url_from_env: string;
}

export type RemoteSchemaRelationship = {
  name: string;
  remote_schema: RemoteSchemaName;
  type: string;
} & (
  | {
      definition: {
        to_remote_schema: {
          remote_schema: RemoteSchemaName;
          lhs_fields: string[];
          remote_field: RemoteField;
        };
      };
    }
  | {
      definition: {
        to_source: {
          relationship_type: 'array' | 'object';
          source: string;
          table: Table;
          field_mapping: Record<string, string>;
        };
      };
    }
);

export type RemoteSchema = {
  name: RemoteSchemaName;
  definition: RemoteSchemaDefinitionWithUrl | RemoteSchemaDefinitionWithEnv;
  comment?: string;
  permissions?: RemoteSchemaPermission[];
  remote_relationships?: RemoteSchemaRelationship[];
};
