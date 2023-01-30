import { Table } from '@/features/hasura-metadata-types';

type BasicRelationshipDetails = {
  name: string;
  fromSource: string;
  fromTable: Table;
};

export type RemoteDatabaseRelationship = BasicRelationshipDetails & {
  type: 'remoteDatabaseRelationship';
  relationshipType: 'Array' | 'Object';
  definition: {
    toSource: string;
    toTable: Table;
    mapping: Record<string, string>;
  };
};

export type RemoteSchemaRelationship = BasicRelationshipDetails & {
  type: 'remoteSchemaRelationship';
  relationshipType: 'Remote';
  definition: {
    toRemoteSchema: string;
    lhs_fields: string[];
    remote_field: Record<string, any>;
  };
};

export type LocalRelationship = BasicRelationshipDetails & {
  type: 'localRelationship';
  relationshipType: 'Array' | 'Object';
  definition: {
    toTable: Table;
    mapping: Record<string, string>;
  };
};

export type Relationship =
  | LocalRelationship
  | RemoteDatabaseRelationship
  | RemoteSchemaRelationship;

export enum MODE {
  CREATE = 'create',
  EDIT = 'edit',
  DELETE = 'delete',
  RENAME = 'rename',
}
