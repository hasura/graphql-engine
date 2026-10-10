import { GraphQLError } from 'graphql';
import { ClientHeader, Table } from '@hasura/shared/types';

export type Definition = {
  sdl: string;
  error?: GraphQLError | null;
  timer?: NodeJS.Timeout | null;
  ast?: Record<string, any> | null;
};

export type ActionExecution = 'synchronous' | 'asynchronous';

export type ActionState = {
  handler: string;
  actionDefinition: Definition;
  typeDefinition: Definition;
  headers: ClientHeader[];
  forwardClientHeaders: boolean;
  kind: ActionExecution;
  derive?: { operation: string };
  timeout: string;
  comment: string;
};

export type CustomTypeRelationshipFieldMapping = {
  field: string;
  column: string;
};

export type CustomTypeObjectRelationshipFormState = {
  name: string;
  type: 'object' | 'array';
  remote_table: Table | undefined;
  field_mapping: CustomTypeRelationshipFieldMapping[];
  source?: string;
};
