import { Table } from '../../../hasura-metadata-types';

export type SuggestedRelationship = {
  type: 'object' | 'array';
  from: {
    table: Table;
    columns: string[];
    constraint_name?: string;
  };
  to: {
    table: Table;
    columns: string[];
    constraint_name?: string;
  };
};

export type SuggestedRelationshipWithName = SuggestedRelationship & {
  constraintName: string;
  id: string;
};

export type SuggestedRelationshipsResponse = {
  relationships: SuggestedRelationship[];
};

export type TrackedSuggestedRelationship = {
  name: string;
  fromTable: Table;
  toTable: Table;
  columnMapping: Record<string, string>;
  type: 'object' | 'array';
};
export const OBJECT_REL_TYPE: TrackedSuggestedRelationship['type'] = 'object';
export const ARRAY_REL_TYPE: TrackedSuggestedRelationship['type'] = 'array';
