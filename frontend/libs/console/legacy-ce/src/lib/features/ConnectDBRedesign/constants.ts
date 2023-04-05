import { DriverInfo } from '../DataSource';
import { EELiteAccess } from '../EETrial';

export const DEFAULT_DRIVER: DriverInfo = {
  name: 'postgres',
  displayName: 'Postgres',
  release: 'GA',
  native: true,
  enterprise: false,
};
export const eeCardContentMap = (
  dbName: string
): Record<
  Extract<
    EELiteAccess['access'],
    'eligible' | 'expired' | 'deactivated' | 'loading'
  >,
  { cardTitle: string; cardText: string }
> => ({
  eligible: {
    cardTitle: `Looking to connect to ${dbName} database?`,
    cardText:
      'Deploy data connectors to add data sources such as Snowflake, Amazon Athena, and more to your GraphQL API.',
  },
  expired: {
    cardTitle: 'Enterprise License Expired',
    cardText:
      'With an Enterprise Edition license you can add data sources such as Snowflake, Amazon Athena, and more to your GraphQL API.',
  },
  deactivated: {
    cardTitle: 'Enterprise License Deactivated',
    cardText:
      'With an Enterprise Edition license you can add data sources such as Snowflake, Amazon Athena, and more to your GraphQL API.',
  },
  loading: {
    cardTitle: '',
    cardText: '',
  },
});
