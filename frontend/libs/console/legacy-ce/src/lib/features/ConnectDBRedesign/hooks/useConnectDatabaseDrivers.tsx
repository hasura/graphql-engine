import { sortBy, uniqBy } from 'lodash'; // eslint-disable-line @typescript-eslint/no-restricted-imports
import { useAvailableDrivers } from '../../ConnectDB/hooks';
import { DriverInfo } from '../../DataSource';
import { DatabaseLogo } from '../components';
import dbLogos from '../graphics/db-logos';

type useDatabaseConnectDriversProps = {
  onFirstSuccess?: (data: DriverInfo[]) => void;
  showEnterpriseDrivers?: boolean;
};

// a GDC driver is only "available" once an agent is added for it
// these are drivers are a special case bc we may want to display them in the UI before their agent's are added in certain cases
const SuperConnectorDrivers: readonly DriverInfo[] = [
  {
    name: 'mysqlgdc',
    displayName: 'MySQL',
    native: false,
    release: 'Alpha',
    enterprise: true,
  },
  {
    name: 'snowflake',
    displayName: 'Snowflake',
    native: false,
    release: 'Beta',
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
    release: 'Beta',
    enterprise: true,
  },
  {
    name: 'oracle',
    displayName: 'Oracle',
    native: false,
    release: 'Beta',
    enterprise: true,
  },
] as const;

// this is a wrapper around useAvailableDrivers
export const useDatabaseConnectDrivers = ({
  showEnterpriseDrivers = true,
  onFirstSuccess,
}: useDatabaseConnectDriversProps = {}) => {
  const { data: availableDrivers } = useAvailableDrivers({
    onFirstSuccess,
  });

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
          image={dbLogos[d.name] || dbLogos.default}
          releaseName={d.release}
        />
      ),
    }));

  return { cardData, allDrivers, availableDrivers };
};
