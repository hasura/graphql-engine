import React, { useState, useEffect } from 'react';
import Helmet from 'react-helmet';
import { connect, ConnectedProps } from 'react-redux';

import Button from '../../../Common/Button/Button';
import styles from './styles.scss';
import { Dispatch, ReduxState } from '../../../../types';
import BreadCrumb from '../../../Common/Layout/BreadCrumb/BreadCrumb';
import { DataSource } from '../../../../metadata/types';
import { Driver } from '../../../../dataSources';
import {
  removeDataSource,
  reloadDataSource,
} from '../../../../metadata/actions';
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
import globals from '../../../../Globals';
import VPCBanner from '../../../Common/VPCBanner/VPCBanner';

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
      <td className="px-sm py-xs max-w-xs align-top w-0 whitespace-nowrap">
        <Button
          size="xs"
          color="white"
          className="mr-xs"
          onClick={viewDB}
          disabled={isInconsistentDataSource}
        >
          View Database
        </Button>
        <Button
          size="xs"
          color="white"
          className="mr-xs"
          onClick={() => {
            setReloading(true);
            onReload(dataSource.name, dataSource.driver, () =>
              setReloading(false)
            );
          }}
        >
          {reloading ? 'Reloading...' : 'Reload'}
        </Button>
        <Button
          size="xs"
          color="white"
          className="mr-xs"
          onClick={() => {
            onEdit(dataSource.name);
          }}
        >
          Edit
        </Button>
        <Button
          size="xs"
          color="white"
          className="text-red-600"
          onClick={() => {
            setRemoving(true);
            onRemove(dataSource.name, dataSource.driver, () =>
              setRemoving(false)
            );
          }}
        >
          {removing ? 'Removing...' : 'Remove'}
        </Button>
      </td>
      <td className="px-sm py-xs max-w-xs align-top">
        <CollapsibleToggle dataSource={dataSource} dbVersion={dbVersion} />
        {isInconsistentDataSource && (
          <ToolTip
            id={`inconsistent-source-${dataSource.name}`}
            placement="right"
            message="Inconsistent Data Source"
          >
            <i
              className="fa fa-exclamation-triangle ml-xs text-red-800"
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
            <i className="fa fa-eye" aria-hidden="true" />
            <p className="ml-xs">Show Connection String</p>
          </span>
        )}
        {showUrl && (
          <ToolTip
            id="connection-string-hide"
            placement="top"
            message="Hide connection string"
          >
            <i
              className="ml-xs cursor-pointer fa fa-times"
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
}) => {
  useEffect(() => {
    if (dataSources.length === 0 && !autoRedirectedToConnectPage) {
      dispatch(_push('/data/manage/connect'));
      autoRedirectedToConnectPage = true;
    }
  }, [location, dataSources, dispatch]);
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
      <div className={`container-fluid ${styles.manage_dbs_page}`}>
        <BreadCrumb breadCrumbs={crumbs} />
        <div className={styles.padd_top}>
          <div className={`${styles.display_flex} manage-db-header`}>
            <h2
              className={`${styles.headerText} ${styles.display_inline} ${styles.add_mar_right}`}
            >
              Data Manager
            </h2>
            <Button
              color="yellow"
              size="md"
              className={styles.add_mar_right}
              onClick={onClickConnectDB}
            >
              Connect Database
            </Button>
          </div>
          {globals.consoleType === 'cloud' && !globals.eeMode && (
            <VPCBanner className="mt-md" />
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
                {dataSources.length ? (
                  dataSources.map(data => (
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
                  ))
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
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);
type InjectedProps = ConnectedProps<typeof connector>;
const ConnectedDatabaseManagePage = connector(ManageDatabase);
export default ConnectedDatabaseManagePage;
