import { GraphQLCustomization } from './components/GraphQLCustomization/schema';

export type DatabaseConnection = {
  driver: string;
  details: {
    name: string;
    configuration: Record<string, any>; // This can vary from driver to driver
    customization: Record<string, any>;
  };
};

export type DatabaseKind =
  | 'postgres'
  | 'mssql'
  | 'bigquery'
  | 'citus'
  | 'alloydb'
  | 'snowflake'
  | 'athena'
  | 'cockroach';

export type EEState = 'active' | 'inactive' | 'expired';
