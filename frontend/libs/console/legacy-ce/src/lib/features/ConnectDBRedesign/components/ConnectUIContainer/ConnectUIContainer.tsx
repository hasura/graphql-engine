import BreadCrumb from '../../../../components/Common/Layout/BreadCrumb/BreadCrumb';
import { getRoute } from '../../../../utils/getDataRoute';
import { ConnectBigQueryWidget } from '../ConnectBigQueryWidget/ConnectBigQueryWidget';
import { ConnectGDCSourceWidget } from '../ConnectGDCSourceWidget/ConnectGDCSourceWidget';
import { ConnectMssqlWidget } from '../ConnectMssqlWidget/ConnectMssqlWidget';
import { ConnectPostgresWidget } from '../ConnectPostgresWidget/ConnectPostgresWidget';

const getDataSourceNameFromUrlParams = (): string | undefined => {
  const urlParams = new URLSearchParams(window.location.search);

  const database = urlParams.get('database');

  return database ?? undefined;
};

const getDriverNameFromUrlParams = (): string | undefined => {
  const urlParams = new URLSearchParams(window.location.search);

  const driver = urlParams.get('driver');

  return driver ?? undefined;
};

const ConnectDatabaseWrapper = () => {
  const dataSourceName = getDataSourceNameFromUrlParams();
  const driver = getDriverNameFromUrlParams();

  if (!driver) return <div>Error. No driver found.</div>;

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
  const driver = getDriverNameFromUrlParams();
  return (
    <div className="p-4">
      <BreadCrumb
        breadCrumbs={[
          {
            url: '/data',
            title: 'Data',
          },
          {
            url: '/data/manage',
            title: 'Manage',
          },
          {
            url: '/data/v2/manage/connect',
            title: 'Connect',
          },
          {
            url: getRoute().connectDatabase(driver),
            title: driver ?? '',
          },
        ]}
      />
      <ConnectDatabaseWrapper />
    </div>
  );
};
