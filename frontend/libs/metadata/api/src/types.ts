import { FetchJson } from '@hasura/shared/utils';
import type { IntrospectionQuery } from 'graphql';

export interface NetworkArgs<T = any> {
  url: string;
  fetchJson: FetchJson<T>;
}

export interface SchemaResponse {
  data: IntrospectionQuery;
}

export interface TableRelationship {
  name: string;
  comment: string;
  type: 'object' | 'array';
  from: {
    table: string;
    column: string[];
  };
  to: {
    table: string;
    column: string[];
  };
}
