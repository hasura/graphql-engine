import { sortBy, uniqBy } from 'lodash'; // eslint-disable-line @typescript-eslint/no-restricted-imports
import { useAvailableDrivers } from '../../ConnectDB/hooks';
import { DriverInfo } from '../../DataSource';
import { DatabaseLogo } from '../components';
import dbLogos from '../graphics/db-logos';
import { SuperConnectorDrivers as SuperDrivers } from '../../hasura-metadata-types';

type useDatabaseConnectDriversProps = {
  onFirstSuccess?: (data: DriverInfo[]) => void;
  showEnterpriseDrivers?: boolean;
};

export const kindNameMap: Record<SuperDrivers, string> = {
  sqlite: 'Hasura SQLite',
  athena: 'Amazon Athena',
  snowflake: 'Snowflake',
  mysql8: 'MySql',
  mariadb: 'MariaDB',
  oracle: 'Oracle',
};

// a GDC driver is only "available" once an agent is added for it
// these are drivers are a special case bc we may want to display them in the UI before their agent's are added in certain cases
const SuperConnectorDrivers: readonly DriverInfo[] = [
  {
    name: 'mysql8',
    displayName: 'MySQL',
    native: false,
    release: 'GA',
    enterprise: true,
  },
  {
    name: 'snowflake',
    displayName: 'Snowflake',
    native: false,
    release: 'GA',
    enterprise: true,
  },
  {
    name: 'athena',
    displayName: 'Amazon Athena',
    native: false,
    release: 'Beta',
    enterprise: true,
  },
  {
    name: 'mariadb',
    displayName: 'MariaDB',
    native: false,
    release: 'GA',
    enterprise: true,
  },
  {
    name: 'oracle',
    displayName: 'Oracle',
    native: false,
    release: 'GA',
    enterprise: true,
  },
  // {
  //   name: 'Mongo',
  //   displayName: 'MongoDB',
  //   native: false,
  //   release: 'Alpha',
  //   enterprise: true,
  // },
] as const;

// this is a wrapper around useAvailableDrivers
export const useDatabaseConnectDrivers = ({
  showEnterpriseDrivers = true,
  onFirstSuccess,
}: useDatabaseConnectDriversProps = {}) => {
  const { data } = useAvailableDrivers({
    onFirstSuccess,
  });

  const availableDrivers = data?.map(d => ({
    ...d,
    displayName: d.displayName || kindNameMap[d.name] || d.name,
  }));

  const allDrivers = sortBy(
    uniqBy(
      [...(availableDrivers ?? []), ...SuperConnectorDrivers],
      d => d.name
    ),
    d => d.displayName
  );

  const cardData = allDrivers
    .filter(d => d.enterprise !== true || showEnterpriseDrivers)
    .map(d => ({
      value: d.name,
      content: (
        <DatabaseLogo
          title={d.displayName}
          noConnection={d.available === false}
          image={dbLogos[d.name] || dbLogos.default}
          releaseName={d.release}
        />
      ),
    }));

  return { cardData, allDrivers, availableDrivers };
};
