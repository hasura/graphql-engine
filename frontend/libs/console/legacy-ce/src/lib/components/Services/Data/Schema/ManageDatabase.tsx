/* eslint-disable no-underscore-dangle */
import React, { useState, useEffect } from 'react';
import Helmet from 'react-helmet';
import { connect, ConnectedProps } from 'react-redux';
import {
  FaExclamationTriangle,
  FaEye,
  FaTimes,
  FaHourglassHalf,
  FaRedoAlt,
  FaExternalLinkAlt,
} from 'react-icons/fa';
import { browserHistory } from 'react-router';
import produce from 'immer';
import { ManageAgents } from '../../../../features/ManageAgents';
import { Button } from '../../../../new-components/Button';
import { useMetadataSource } from '../../../../features/MetadataAPI';
import { Analytics, REDACT_EVERYTHING } from '../../../../features/Analytics';
import { nativeDrivers } from '../../../../features/DataSource';
import { getProjectId } from '../../../../utils/cloudConsole';
import {
  CheckDatabaseLatencyResponse,
  useCheckDatabaseLatency,
  useUpdateProjectRegionChangeStat,
} from '../../../../features/ConnectDB';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { isCloudConsole } from '../../../../utils';
import globals from '../../../../Globals';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';

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
import {
  checkHighLatencySources,
  DBLatencyData,
  getSourceInfoFromLatencyData,
  useVPCBannerVisibility,
} from './utils';
import { NeonDashboardLink } from '../DataSources/CreateDataSource/Neon/components/NeonDashboardLink';

const KNOW_MORE_PROJECT_REGION_UPDATE =
  'https://hasura.io/docs/latest/projects/regions/#changing-region-of-an-existing-project';

type DatabaseListItemProps = {
  dataSource: DataSource;
  inconsistentObjects: InjectedProps['inconsistentObjects'];
  onEdit: (dbName: string) => void;
  onReload: (name: string, driver: Driver, cb: () => void) => void;
  onRemove: (name: string, driver: Driver, cb: () => void) => void;
  pushRoute: (route: string) => void;
  dispatch: Dispatch;
  dataHeaders: Record<string, string>;
  dbLatencyData?: DBLatencyData;
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
  dbLatencyData,
}) => {
  const [reloading, setReloading] = useState(false);
  const [removing, setRemoving] = useState(false);
  const [showUrl, setShowUrl] = useState(false);
  const [dbVersion, setDbVersion] = useState('');
  const { data: sourceInfo } = useMetadataSource(dataSource?.name);

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

  const isTotalMaxConnectionSet =
    !!sourceInfo?.configuration?.connection_info?.pool_settings
      ?.total_max_connections;
  const isMaxConnectionSet =
    !!sourceInfo?.configuration?.connection_info?.pool_settings
      ?.max_connections;
  const showMaxConnectionWarning =
    isCloudConsole(globals) && !isTotalMaxConnectionSet && isMaxConnectionSet;

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
        {showMaxConnectionWarning && (
          <span
            className="bg-blue-100 font-bold rounded-lg pr-xs cursor-pointer"
            onClick={() => {
              onEdit(dataSource.name);
            }}
          >
            <FaExclamationTriangle className="mr-0.5 pb-1 pl-1.5 text-lg" />
            Set Total Max Connections
          </span>
        )}
      </td>
      <td className="px-sm py-xs max-w-xs align-center">
        <CollapsibleToggle
          dataSource={dataSource}
          dbVersion={dbVersion}
          dbLatencyData={dbLatencyData}
        />
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
      <td className="px-sm py-xs max-w-xs align-center break-all">
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
              data-test={`remove-${dataSource.name}`}
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

type ManageDatabaseProps = InjectedProps;

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

  const isPgMssqlSourceConnected = sourcesFromMetadata.some(
    source => source.kind === 'mssql' || source.kind === 'postgres'
  );

  const [fireLatencyRequest, setFireLatencyRequest] = useState(false);

  const queryResponse = useCheckDatabaseLatency(fireLatencyRequest);

  const checkDatabaseLatency = () => {
    setFireLatencyRequest(true);
  };

  const insertProjectRegionChangeStatMutation =
    useUpdateProjectRegionChangeStat();

  const openUpdateProjectRegionPage = () => {
    const projectId = getProjectId(globals);
    if (!projectId) {
      return;
    }
    insertProjectRegionChangeStatMutation.mutate(queryResponse.data);

    const cloudDetailsPage = `${window.location.protocol}//${window.location.host}/project/${projectId}/details?open_update_region_drawer=true`;

    window.open(cloudDetailsPage, '_blank');
  };

  const [showCheckLatencyButton, setLatencyButtonVisibility] = useState(
    sourcesFromMetadata.length !== 0 &&
      isPgMssqlSourceConnected &&
      isCloudConsole(globals)
  );

  const [latencyCheckData, setLatencyCheckData] = useState<
    CheckDatabaseLatencyResponse | undefined
  >(undefined);

  const [showErrorIndicator, setShowErrorIndicator] = useState(false);

  useEffect(() => {
    if (queryResponse.isSuccess && typeof queryResponse.data !== 'string') {
      setFireLatencyRequest(false);
      setLatencyButtonVisibility(false);
      setLatencyCheckData(queryResponse.data);
    } else if (typeof queryResponse.data === 'string') {
      setShowErrorIndicator(true);
    }
    setFireLatencyRequest(false);
  }, [queryResponse.isSuccess, queryResponse.data]);

  const showAccelerateProjectSection =
    isCloudConsole(globals) &&
    !showCheckLatencyButton &&
    !checkHighLatencySources(latencyCheckData) &&
    (!queryResponse.isLoading || !queryResponse.isFetching);

  useEffect(() => {
    if (!location.search.includes('trigger_db_latency_check')) {
      return;
    }

    setFireLatencyRequest(true);

    const newLocation = produce(browserHistory.getCurrentLocation(), draft => {
      // NOTE: the next few lines will help remove the query param once we trigger
      // a request to check the db latency to avoid situations where might
      // end up rendering the page in loops
      delete draft.query.trigger_db_latency_check;
    });

    browserHistory.replace(newLocation);
  }, []);

  return (
    <RightContainer>
      <Helmet title="Manage - Data | Hasura" />
      <Analytics name="ActionEditor" {...REDACT_EVERYTHING}>
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
                            dbLatencyData={
                              isCloudConsole(globals)
                                ? getSourceInfoFromLatencyData(
                                    data.name,
                                    latencyCheckData
                                  )
                                : undefined
                            }
                          />
                        );
                      }
                      return (
                        <GDCDatabaseListItem
                          dataSource={{
                            name: source.name,
                            kind: source.kind,
                          }}
                          inconsistentObjects={inconsistentObjects}
                          dispatch={dispatch}
                        />
                      );
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

          {showCheckLatencyButton ? (
            <Button
              size="md"
              className="mt-xs mr-xs"
              icon={<FaHourglassHalf />}
              onClick={checkDatabaseLatency}
              isLoading={queryResponse.isLoading}
              loadingText="Measuring Latencies..."
            >
              Check Database Latency
            </Button>
          ) : null}
          {showAccelerateProjectSection ? (
            <div className="mt-xs">
              <IndicatorCard
                status="negative"
                headline="Accelerate your Hasura Project"
              >
                <div className="flex items-center flex-row">
                  <span>
                    Databases marked with “Elevated Latency” indicate that it
                    took us over 200 ms for this Hasura project to communicate
                    with your database. These conditions generally happen when
                    databases and projects are in geographically distant
                    regions. This can cause API and subsequently application
                    performance issues. We want your GraphQL APIs to be{' '}
                    <b>lightning fast</b>, therefore we recommend that you
                    either deploy your Hasura project in the same region as your
                    database or select a database instance that&apos;s closer to
                    where you&apos;ve deployed Hasura.
                    <LearnMoreLink href={KNOW_MORE_PROJECT_REGION_UPDATE} />
                  </span>
                  <div className="flex items-center flex-row ml-xs">
                    <Button
                      className="mr-xs"
                      onClick={checkDatabaseLatency}
                      isLoading={queryResponse.isLoading}
                      loadingText="Measuring Latencies..."
                      icon={<FaRedoAlt />}
                    >
                      Re-check Database Latency
                    </Button>
                    <Button
                      className="mr-xs"
                      onClick={openUpdateProjectRegionPage}
                      icon={<FaExternalLinkAlt />}
                    >
                      Update Project Region
                    </Button>
                  </div>
                </div>
              </IndicatorCard>
            </div>
          ) : null}
          {showErrorIndicator ? (
            <div className="mt-xs">
              <IndicatorCard
                status="negative"
                headline="Houston, we've got a problem here!"
                showIcon
              >
                <div className="flex items-center flex-row">
                  <span>
                    There was an error in fetching the latest latency data.
                    <pre className="w-1/2">{queryResponse.data}</pre>
                  </span>
                  <div className="flex items-center flex-row ml-xs">
                    <Button
                      className="mr-xs"
                      onClick={checkDatabaseLatency}
                      isLoading={queryResponse.isLoading}
                      loadingText="Measuring Latencies..."
                    >
                      Re-check Database Latency
                    </Button>
                    <Button onClick={() => setLatencyButtonVisibility(true)}>
                      Close
                    </Button>
                  </div>
                </div>
              </IndicatorCard>
            </div>
          ) : null}
          <NeonDashboardLink className="mt-lg" />

          <div className="mt-lg">
            <ManageAgents />
          </div>
        </div>
      </Analytics>
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
