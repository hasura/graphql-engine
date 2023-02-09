import React from 'react';
import { DatabaseLogo } from './components';
import postgresLogo from './graphics/db-logos/postgres.svg';
import googleLogo from './graphics/db-logos/google.svg';
import microsoftLogo from './graphics/db-logos/microsoft.svg';
import citusLogo from './graphics/db-logos/citus.svg';
import cockroachLogo from './graphics/db-logos/cockroach.svg';
import amazonLogo from './graphics/db-logos/amazon.svg';
import snowflakeLogo from './graphics/db-logos/snowflake.svg';
import { DatabaseKind } from '../../types';

export const dbDisplayNames: Record<DatabaseKind, string> = {
  postgres: 'PostgresSQL',
  citus: 'Citus',
  cockroach: 'CockroachDB',
  alloydb: 'AlloyDB',
  mssql: 'MSSQL',
  bigquery: 'BigQuery',
  snowflake: 'Snowflake',
  athena: 'Amazon Athena',
};

export const databases: { value: DatabaseKind; content: React.ReactNode }[] = [
  {
    value: 'postgres',
    content: (
      <DatabaseLogo title={dbDisplayNames.postgres} image={postgresLogo} />
    ),
  },
  {
    value: 'citus',
    content: <DatabaseLogo title={dbDisplayNames.citus} image={citusLogo} />,
  },
  {
    value: 'cockroach',
    content: (
      <DatabaseLogo title={dbDisplayNames.cockroach} image={cockroachLogo} />
    ),
  },
  {
    value: 'alloydb',
    content: <DatabaseLogo title={dbDisplayNames.alloydb} image={googleLogo} />,
  },
  {
    value: 'mssql',
    content: (
      <DatabaseLogo title={dbDisplayNames.mssql} image={microsoftLogo} />
    ),
  },
  {
    value: 'bigquery',
    content: (
      <DatabaseLogo title={dbDisplayNames.bigquery} image={googleLogo} />
    ),
  },
  {
    value: 'snowflake',
    content: (
      <DatabaseLogo title={dbDisplayNames.snowflake} image={snowflakeLogo} />
    ),
  },
  {
    value: 'athena',
    content: <DatabaseLogo title={dbDisplayNames.athena} image={amazonLogo} />,
  },
];
