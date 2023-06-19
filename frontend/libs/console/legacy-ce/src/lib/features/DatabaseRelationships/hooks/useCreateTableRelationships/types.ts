import { Table } from '../../../hasura-metadata-types';
export type TableRelationshipBasicDetails = {
  driver: string;
  name: string;
  source: { fromSource: string; fromTable: Table };
  isEditMode?: boolean;
};

export type LocalTableRelationshipDefinition = {
  target: {
    toSource: string;
    toTable: Table;
  };
  type: 'array' | 'object';
  detail:
    | {
        fkConstraintOn: 'fromTable' | 'toTable';
        fromColumns: string[];
        toColumns: string[];
      }
    | { columnMapping: Record<string, string> };
};

export type RemoteSchemaRelationshipDefinition = {
  target: {
    toRemoteSchema: string;
  };
  detail: { lhs_fields: string[]; remote_field: Record<string, any> };
};

export type RemoteTableRelationshipDefinition = {
  target: {
    toRemoteSource: string;
    toRemoteTable: Table;
  };
  type: 'array' | 'object';
  detail: { columnMapping: Record<string, string> };
};

export type AllowedRelationshipDefinitions =
  | LocalTableRelationshipDefinition
  | RemoteSchemaRelationshipDefinition
  | RemoteTableRelationshipDefinition;

export type CreateTableRelationshipRequestBodyProps =
  TableRelationshipBasicDetails & {
    definition: AllowedRelationshipDefinitions;
    targetCapabilities: {
      isLocalTableRelationshipSupported: boolean;
      isRemoteTableRelationshipSupported: boolean;
      isRemoteSchemaRelationshipSupported: boolean;
    };
    sourceCapabilities: {
      isLocalTableRelationshipSupported: boolean;
      isRemoteTableRelationshipSupported: boolean;
      isRemoteSchemaRelationshipSupported: boolean;
    };
  };

export type RenameRelationshipProps = {
  name: string;
  driver: string;
  new_name: string;
  source: string;
  table: Table;
};

export type DeleteRelationshipProps = {
  driver: string;
  name: string;
  table: Table;
  source: string;
  isRemote: boolean;
};
