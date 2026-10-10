import { DataQueryType } from './permissions';
import { Table } from './source';

export type InconsistentObjectRequest = {
  proxy: string | null;
  secure: boolean;
  path: string;
  responseTimeout: string;
  method: 'POST' | 'GET' | 'PUT' | 'DELETE' | 'PATCH' | 'OPTION';
  host: string;
  requestVersion: `${number}`;
  redirectCount: `${number}`;
  port: `${number}`;
};

export type InconsistentObjectBaseFields = {
  reason: string;
  name: string;
  message?:
    | string
    | {
        message: string;
        request: InconsistentObjectRequest;
      };
};

export type InconsistentObjectInheritedRole = InconsistentObjectBaseFields & {
  type: 'inherited role permission inconsistency';
  entity:
    | {
        permission_type: DataQueryType;
        source: string;
        table: string;
      }
    | {
        remote_schema: string;
      };
};

export type InconsistentObjectTable = InconsistentObjectBaseFields & {
  type: 'table';
  definition: Table;
  function_name: string;
};

export type InconsistentObjectArrayRelation = InconsistentObjectBaseFields & {
  type: 'array_relation';
  definition: {
    name: string;
    source: string;
    comment: string;
    table: Table;
    using: {
      foreign_key_constraint_on?: {
        column: string;
        table: Table;
      };
    };
  };
};

export type InconsistentObjectRemoteRelationship =
  InconsistentObjectBaseFields & {
    type: 'remote_relationship';
    definition: {
      remote_schema: string;
      name: string;
      table: Table;
    };
  };

export type InconsistentObjectRemoteSchema = InconsistentObjectBaseFields & {
  type: 'remote_schema';
  definition: {
    name: string;
    definition: {
      url?: string;
      url_from_env?: string;
    };
  };
};

export type InconsistentObjectRelation = InconsistentObjectBaseFields & {
  type: 'object_relation';
  definition: {
    name: string;
    table: Table;
  };
};

export type InconsistentObjectEventTrigger = InconsistentObjectBaseFields & {
  type: 'event_trigger';
  definition: {
    configuration: {
      name: string;
    };
    table: Table;
  };
};

export type InconsistentObjectPermission = InconsistentObjectBaseFields & {
  type:
    | 'select_permission'
    | 'update_permission'
    | 'insert_permission'
    | 'delete_permission';
  definition: {
    role: string;
    table: string;
  };
};

export type InconsistentSource = InconsistentObjectBaseFields & {
  type: 'source';
  definition: string;
};

export type InconsistentObjectOther = InconsistentObjectBaseFields & {
  type:
    | 'function'
    | 'action'
    | 'native_query'
    | 'remote_schema_permission'
    | 'remote_schema_remote_relationship'
    | 'stored_procedure'
    | 'logical_model'
    | 'computed_field'
    | 'function_permission';
  definition: unknown;
};

export type InconsistentObjectType =
  | InconsistentObjectTable['type']
  | InconsistentObjectArrayRelation['type']
  | InconsistentObjectRemoteRelationship['type']
  | InconsistentObjectRemoteSchema['type']
  | InconsistentObjectRelation['type']
  | InconsistentObjectEventTrigger['type']
  | InconsistentObjectPermission['type']
  | InconsistentObjectOther['type']
  | InconsistentObjectInheritedRole['type']
  | InconsistentSource['type'];

export type InconsistentObjectFields =
  | InconsistentObjectTable
  | InconsistentObjectArrayRelation
  | InconsistentObjectRemoteRelationship
  | InconsistentObjectRemoteSchema
  | InconsistentObjectRelation
  | InconsistentObjectEventTrigger
  | InconsistentObjectPermission
  | InconsistentSource
  | InconsistentObjectOther
  | InconsistentObjectInheritedRole;

export type InconsistentObject =
  | {
      objects: InconsistentObjectFields[];
      reason: string;
    }
  | {
      definitions: InconsistentObjectFields[];
      reason: string;
    }
  | {
      conflicts: InconsistentObjectFields[];
      reason: string;
    }
  | InconsistentObjectFields;

export type InconsistentMetadata = {
  inconsistent_objects: InconsistentObject[];
  is_consistent: boolean;
};
