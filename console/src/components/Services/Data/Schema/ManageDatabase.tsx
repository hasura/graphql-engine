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
    <div
      className={`${styles.flex_space_between} ${styles.add_pad_min} ${styles.db_list_item}`}
      data-test={dataSource.name}
    >
      <div className={`${styles.display_flex_container} ${styles.flex_width}`}>
        <Button
          size="xs"
          color="white"
          className={styles.add_mar_right_mid}
          onClick={viewDB}
          disabled={isInconsistentDataSource}
        >
          View Database
        </Button>
        <Button
          size="xs"
          color="white"
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
          onClick={() => {
            onEdit(dataSource.name);
          }}
          className={styles.add_mar_left_mid}
        >
          Edit
        </Button>
        <Button
          className={`${styles.text_red}`}
          size="xs"
          color="white"
          onClick={() => {
            setRemoving(true);
            onRemove(dataSource.name, dataSource.driver, () =>
              setRemoving(false)
            );
          }}
          style={{ marginLeft: '10px' }}
        >
          {removing ? 'Removing...' : 'Remove'}
        </Button>
        <div
          className={`${styles.flexColumn} ${styles.container_max_width} ${styles.displayFlexContainer} ${styles.add_pad_left}`}
        >
          <CollapsibleToggle dataSource={dataSource} dbVersion={dbVersion} />
        </div>
        {isInconsistentDataSource && (
          <ToolTip
            id={`inconsistent-source-${dataSource.name}`}
            placement="right"
            message="Inconsistent Data Source"
          >
            <i
              className={`fa fa-exclamation-triangle ${styles.inconsistentSourceIcon}`}
              aria-hidden="true"
            />
          </ToolTip>
        )}
      </div>
      <div className={`${styles.db_large_string_break_words}`}>
        {showUrl ? (
          typeof dataSource.url === 'string' ? (
            dataSource.url
          ) : (
            dataSource.url.from_env
          )
        ) : (
          <span
            className={styles.show_connection_string}
            onClick={() => setShowUrl(true)}
          >
            <i
              className={`${styles.showAdminSecret} fa fa-eye`}
              aria-hidden="true"
            />
            <p style={{ marginLeft: 6 }}>Show Connection String</p>
          </span>
        )}
        {showUrl && (
          <ToolTip
            id="connection-string-hide"
            placement="top"
            message="Hide connection string"
          >
            <i
              className={`${styles.closeHeader} fa fa-times`}
              aria-hidden="true"
              onClick={() => setShowUrl(false)}
              style={{ paddingLeft: 10 }}
            />
          </ToolTip>
        )}
      </div>
    </div>
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
        </div>
        <div className={styles.manage_db_content}>
          <hr className="my-md" />
          <h3 className={styles.heading_text}>Connected Databases</h3>
          <div className={styles.flexColumn}>
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
              <span style={{ paddingTop: 15 }}>
                You don&apos;t have any data sources connected.
              </span>
            )}
          </div>
          <hr className="my-md" />
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
