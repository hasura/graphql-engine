import React from 'react';
import PlaceholderLogo from '../../graphics/db-logos/placeholder.svg';
import PostgresLogo from '../../graphics/db-logos/postgres.svg';
import { DatabaseLogo } from './components/DatabaseLogo';
import { DatabaseKind } from './types';

export const databases: { value: DatabaseKind; content: React.ReactNode }[] = [
  {
    value: 'postgres',
    content: <DatabaseLogo title="PostgreSQL" image={PostgresLogo} />,
  },
  {
    value: 'citus',
    content: <DatabaseLogo title="Citus" image={PlaceholderLogo} />,
  },
  {
    value: 'cockroach',
    content: <DatabaseLogo title="CockroachDB" image={PlaceholderLogo} />,
  },
  {
    value: 'alloydb',
    content: <DatabaseLogo title="AlloyDB" image={PlaceholderLogo} />,
  },
  {
    value: 'mssql',
    content: <DatabaseLogo title="MSSQL" image={PlaceholderLogo} />,
  },
  {
    value: 'bigquery',
    content: <DatabaseLogo title="BigQuery" image={PlaceholderLogo} />,
  },
  {
    value: 'snowflake',
    content: <DatabaseLogo title="Snowflake" image={PlaceholderLogo} />,
  },
  {
    value: 'athena',
    content: <DatabaseLogo title="Amazon Athena" image={PlaceholderLogo} />,
  },
];
