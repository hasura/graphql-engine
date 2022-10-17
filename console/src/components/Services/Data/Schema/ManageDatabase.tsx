/* eslint-disable no-underscore-dangle */
import React, { useState, useEffect } from 'react';
import Helmet from 'react-helmet';
import { connect, ConnectedProps } from 'react-redux';
import { FaExclamationTriangle, FaEye, FaTimes } from 'react-icons/fa';
import { ManageAgents } from '@/features/ManageAgents';
import { Button } from '@/new-components/Button';
import {
  availableFeatureFlagIds,
  useIsFeatureFlagEnabled,
} from '@/features/FeatureFlags';
import { nativeDrivers } from '@/features/DataSource';
import { isProConsole } from '@/utils/proConsole';
import styles from './styles.module.scss';
import { Dispatch, ReduxState } from '../../../../types';
import BreadCrumb from '../../../Common/Layout/BreadCrumb/BreadCrumb';
import { DataSource } from '../../../../metadata/types';
import { Driver } from '../../../../dataSources';
import {
  removeDataSource,
  reloadDataSource,
} from '../../../../metadata/actions';
import { GDCDatabaseListItem } from './components/GDCDatabaseListItem';
import { RightContainer } from '../../../Common/Layout/RightContainer';
import { getDataSources } from '../../../../metadata/selector';
import ToolTip from '../../../Common/Tooltip/Tooltip';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import { mapDispatchToPropsEmpty } from '../../../Common/utils/reactUtils';
import _push from '../push';
import { isInconsistentSource } from '../utils';
import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import { getRunSqlQuery } from '../../../Common/utils/v1QueryUtils';
import requestAction from '../../../../utils/requestAction';
import { showErrorNotification } from '../../Common/Notification';
import { services } from '../../../../dataSources/services';
import CollapsibleToggle from './CollapsibleToggle';
import VPCBanner from '../../../Common/VPCBanner/VPCBanner';
import { useVPCBannerVisibility } from './utils';
import { NeonDashboardLink } from '../DataSources/CreateDataSource/Neon/components/NeonDashboardLink';

type DatabaseListItemProps = {
  dataSource: DataSource;
  inconsistentObjects: InjectedProps['inconsistentObjects'];
  onEdit: (dbName: string) => void;
  onReload: (name: string, driver: Driver, cb: () => void) => void;
  onRemove: (name: string, driver: Driver, cb: () => void) => void;
  pushRoute: (route: string) => void;
  dispatch: Dispatch;
  dataHeaders: Record<string, string>;
};

const DatabaseListItem: React.FC<DatabaseListItemProps> = ({
  onEdit,
  pushRoute,
  onReload,
  onRemove,
  dataSource,
  inconsistentObjects,
  dispatch,
  dataHeaders,
}) => {
  const [reloading, setReloading] = useState(false);
  const [removing, setRemoving] = useState(false);
  const [showUrl, setShowUrl] = useState(false);
  const [dbVersion, setDbVersion] = useState('');
  const fetchDBVersion = () => {
    const query = services[dataSource.driver].getDatabaseVersionSql ?? '';

    if (!query) {
      setDbVersion('');
      return;
    }

    const url = Endpoints.query;
    const options: RequestInit = {
      method: 'POST',
      credentials: globalCookiePolicy,
      headers: dataHeaders,
      body: JSON.stringify(
        getRunSqlQuery(query, dataSource.name, false, true, dataSource.driver)
      ),
    };

    dispatch(requestAction(url, options)).then(
      data => {
        setDbVersion(data.result[1][0]);
      },
      error => {
        dispatch(
          showErrorNotification('Failed to fetch Database version', null, error)
        );
      }
    );
  };

  useEffect(() => {
    fetchDBVersion();
  }, []);

  const viewDB = () => {
    if (dataSource?.name) pushRoute(`/data/${dataSource.name}/schema`);
  };
  const isInconsistentDataSource = isInconsistentSource(
    dataSource.name,
    inconsistentObjects
  );

  return (
    <tr data-test={dataSource.name}>
      <td className="px-sm py-xs align-top w-0 whitespace-nowrap">
        <Button
          size="sm"
          className="mr-xs"
          onClick={viewDB}
          disabled={isInconsistentDataSource}
        >
          View Database
        </Button>
        <Button
          size="sm"
          className="mr-xs"
          isLoading={reloading}
          loadingText="Reloading..."
          onClick={() => {
            setReloading(true);
            onReload(dataSource.name, dataSource.driver, () =>
              setReloading(false)
            );
          }}
        >
          Reload
        </Button>
        {isProConsole(window.__env)
          ? !dataSource?.connection_pool_settings?.total_max_connections && (
              <span className="bg-blue-100 font-bold rounded-lg pr-xs">
                <FaExclamationTriangle className="mr-0.5 pb-1 pl-1.5 text-lg" />
                Set Total Max Connections
              </span>
            )
          : null}
      </td>
      <td className="px-sm py-xs max-w-xs align-top">
        <CollapsibleToggle dataSource={dataSource} dbVersion={dbVersion} />
        {isInconsistentDataSource && (
          <ToolTip
            id={`inconsistent-source-${dataSource.name}`}
            placement="right"
            message="Inconsistent Data Source"
          >
            <FaExclamationTriangle
              className="ml-xs text-red-800"
              aria-hidden="true"
            />
          </ToolTip>
        )}
      </td>
      <td className="px-sm py-xs max-w-xs align-top break-all">
        {showUrl ? (
          typeof dataSource.url === 'string' ? (
            dataSource.url
          ) : (
            dataSource.url.from_env
          )
        ) : (
          <span
            className="text-secondary flex items-center cursor-pointer"
            onClick={() => setShowUrl(true)}
          >
            <FaEye aria-hidden="true" />
            <div className="ml-xs mr-4">Show Connection String</div>
            <Button
              size="sm"
              className="mr-xs"
              onClick={() => {
                onEdit(dataSource.name);
              }}
            >
              Edit
            </Button>
            <Button
              size="sm"
              isLoading={removing}
              loadingText="Removing..."
              className="text-red-600"
              onClick={() => {
                setRemoving(true);
                onRemove(dataSource.name, dataSource.driver, () =>
                  setRemoving(false)
                );
              }}
            >
              Remove
            </Button>
          </span>
        )}
        {showUrl && (
          <ToolTip
            id="connection-string-hide"
            placement="top"
            message="Hide connection string"
          >
            <FaTimes
              className="ml-xs cursor-pointer"
              aria-hidden="true"
              onClick={() => setShowUrl(false)}
            />
          </ToolTip>
        )}
      </td>
    </tr>
  );
};

interface ManageDatabaseProps extends InjectedProps {}

let autoRedirectedToConnectPage = false;

const ManageDatabase: React.FC<ManageDatabaseProps> = ({
  dataSources,
  dispatch,
  inconsistentObjects,
  location,
  dataHeaders,
  sourcesFromMetadata,
}) => {
  useEffect(() => {
    if (sourcesFromMetadata.length === 0 && !autoRedirectedToConnectPage) {
      /**
       * Because the getDataSources() doesn't list the GDC sources, the Data tab will redirect to the /connect page
       * thinking that are no sources available in Hasura, even if there are GDC sources connected to it. Modifying getDataSources()
       * to list gdc sources is a huge task that involves modifying redux state variables.
       * So a quick workaround is to check from the actual metadata if any sources are present -
       * Combined with checks between getDataSources() and metadata -> we know the remaining sources are GDC sources. In such a case redirect to the manage db route
       */
      dispatch(_push('/data/manage/connect'));
      autoRedirectedToConnectPage = true;
    }
  }, [location, dataSources, dispatch]);

  const { show: shouldShowVPCBanner, dismiss: dismissVPCBanner } =
    useVPCBannerVisibility();

  const { enabled: isDCAgentsManageUIEnabled } = useIsFeatureFlagEnabled(
    availableFeatureFlagIds.gdcId
  );

  const crumbs = [
    {
      title: 'Data',
      url: `/data/`,
    },
    {
      title: 'Manage',
      url: '#',
    },
  ];

  const onRemove = (name: string, driver: Driver, cb: () => void) => {
    const confirmation = getConfirmation(
      `Your action will remove the "${name}" data source`,
      true,
      name
    );
    if (confirmation) {
      dispatch(removeDataSource({ driver, name }))
        .then(cb)
        .catch(err => {
          console.error(err);
          cb();
        });
      return;
    }
    // in case there was no confirmation
    cb();
  };

  const onReload = (name: string, driver: Driver, cb: () => void) => {
    dispatch(reloadDataSource({ driver, name })).then(cb);
  };

  const onClickConnectDB = () => {
    dispatch(_push('/data/manage/connect'));
  };

  const pushRoute = (route: string) => {
    if (route) dispatch(_push(route));
  };

  const onEdit = (dbName: string) => {
    dispatch(_push(`/data/manage/edit/${dbName}`));
  };

  return (
    <RightContainer>
      <Helmet title="Manage - Data | Hasura" />
      <div
        className={`container-fluid ${styles.manage_dbs_page}`}
        data-test="manage-database-section"
      >
        <BreadCrumb breadCrumbs={crumbs} />
        <div className={styles.padd_top}>
          <div className={`${styles.display_flex} manage-db-header`}>
            <h2
              className={`${styles.headerText} ${styles.display_inline} ${styles.add_mar_right}`}
            >
              Data Manager
            </h2>
            <Button
              mode="primary"
              size="md"
              className={styles.add_mar_right}
              onClick={onClickConnectDB}
            >
              Connect Database
            </Button>
          </div>
          {shouldShowVPCBanner && (
            <VPCBanner className="mt-md" onClose={dismissVPCBanner} />
          )}
        </div>
        <div className={styles.manage_db_content}>
          <hr className="my-md" />

          <div className="overflow-x-auto border border-gray-300 rounded">
            <table className="min-w-full divide-y divide-gray-200">
              <thead className="bg-gray-50">
                <th className="px-sm py-xs max-w-xs text-left text-sm bg-gray-50 font-semibold text-gray-600 uppercase tracking-wider" />
                <th className="px-sm py-xs text-left text-sm bg-gray-50 font-semibold text-gray-600 uppercase tracking-wider">
                  Database
                </th>
                <th className="px-sm py-xs text-left text-sm bg-gray-50 font-semibold text-gray-600 uppercase tracking-wider">
                  Connection String
                </th>
              </thead>
              <tbody className="bg-white divide-y divide-gray-200">
                {sourcesFromMetadata.length ? (
                  sourcesFromMetadata.map(source => {
                    if (nativeDrivers.includes(source.kind)) {
                      const data = dataSources.find(
                        s => s.name === source.name
                      );
                      if (!data) return null;

                      return (
                        <DatabaseListItem
                          key={data.name}
                          dataSource={data}
                          inconsistentObjects={inconsistentObjects}
                          pushRoute={pushRoute}
                          onEdit={onEdit}
                          onReload={onReload}
                          onRemove={onRemove}
                          dispatch={dispatch}
                          dataHeaders={dataHeaders}
                        />
                      );
                    }
                    if (isDCAgentsManageUIEnabled)
                      return (
                        <GDCDatabaseListItem
                          dataSource={{ name: source.name, kind: source.kind }}
                          inconsistentObjects={inconsistentObjects}
                          dispatch={dispatch}
                        />
                      );
                    return null;
                  })
                ) : (
                  <td colSpan={3} className="text-center px-sm py-xs">
                    You don&apos;t have any data sources connected, please
                    connect one to continue.
                  </td>
                )}
              </tbody>
            </table>
          </div>
        </div>

        {isDCAgentsManageUIEnabled ? (
          <div className="mt-lg">
            <ManageAgents />
          </div>
        ) : null}

        <NeonDashboardLink className="mt-lg" />
      </div>
    </RightContainer>
  );
};

const mapStateToProps = (state: ReduxState) => {
  return {
    dataHeaders: state.tables.dataHeaders,
    schemaList: state.tables.schemaList,
    dataSources: getDataSources(state),
    currentDataSource: state.tables.currentDataSource,
    currentSchema: state.tables.currentSchema,
    inconsistentObjects: state.metadata.inconsistentObjects,
    location: state?.routing?.locationBeforeTransitions,
    sourcesFromMetadata: state?.metadata?.metadataObject?.sources ?? [],
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);
type InjectedProps = ConnectedProps<typeof connector>;
const ConnectedDatabaseManagePage = connector(ManageDatabase);
export default ConnectedDatabaseManagePage;
