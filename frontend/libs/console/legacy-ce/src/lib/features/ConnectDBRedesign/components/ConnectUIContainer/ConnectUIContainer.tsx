import { Breadcrumbs, IndicatorCard } from '@hasura/shared/ui';
import { ConnectBigQueryWidget } from '../ConnectBigQueryWidget/ConnectBigQueryWidget';
import { ConnectGDCSourceWidget } from '../ConnectGDCSourceWidget/ConnectGDCSourceWidget';
import { ConnectMssqlWidget } from '../ConnectMssqlWidget/ConnectMssqlWidget';
import { ConnectPostgresWidget } from '../ConnectPostgresWidget/ConnectPostgresWidget';
import { dataRoutes } from '@hasura/shared/utils';
import { useSearchParams } from 'react-router';

const getDataSourceNameFromUrlParams = (
  urlParams: URLSearchParams,
): string | undefined => {
  const database = urlParams.get('database');

  return database ?? undefined;
};

const getDriverNameFromUrlParams = (
  urlParams: URLSearchParams,
): string | undefined => {
  const driver = urlParams.get('driver');

  return driver ?? undefined;
};

const ConnectDatabaseWrapper = () => {
  const [searchParams] = useSearchParams();
  const dataSourceName = getDataSourceNameFromUrlParams(searchParams);
  const driver = getDriverNameFromUrlParams(searchParams);

  if (!driver)
    return (
      <IndicatorCard status="negative" showIcon>
        Error. No driver found.
      </IndicatorCard>
    );

  if (driver === 'postgres')
    return <ConnectPostgresWidget dataSourceName={dataSourceName} />;

  if (driver === 'citus')
    return (
      <ConnectPostgresWidget
        dataSourceName={dataSourceName}
        overrideDisplayName="Citus"
        overrideDriver="citus"
      />
    );

  if (driver === 'alloy')
    return (
      <ConnectPostgresWidget
        dataSourceName={dataSourceName}
        overrideDisplayName="AlloyDB"
      />
    );

  if (driver === 'cockroach')
    return (
      <ConnectPostgresWidget
        dataSourceName={dataSourceName}
        overrideDisplayName="CockroachDB"
        overrideDriver="cockroach"
      />
    );

  if (driver === 'bigquery')
    return <ConnectBigQueryWidget dataSourceName={dataSourceName} />;

  if (driver === 'mssql')
    return <ConnectMssqlWidget dataSourceName={dataSourceName} />;

  return (
    <ConnectGDCSourceWidget dataSourceName={dataSourceName} driver={driver} />
  );
};

export const ConnectUIContainer = () => {
  const [searchParams] = useSearchParams();
  const driver = getDriverNameFromUrlParams(searchParams);

  return (
    <div className="p-6">
      <Breadcrumbs
        className="mb-4"
        items={[
          {
            url: '/data',
            title: 'Data',
          },
          {
            url: '/data/manage',
            title: 'Manage',
          },
          {
            url: dataRoutes.connectDatabase(),
            title: 'Connect',
          },
          {
            url: dataRoutes.connectDatabase(driver),
            title: driver ?? '',
          },
        ]}
      />
      <ConnectDatabaseWrapper />
    </div>
  );
};
