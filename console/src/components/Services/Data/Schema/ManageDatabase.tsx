import { ThunkDispatch } from 'redux-thunk';
import React, { useState } from 'react';
import Helmet from 'react-helmet';

import { connect, ConnectedProps } from 'react-redux';
import Button from '../../../Common/Button/Button';
import styles from '../../../Common/Common.scss';
import { ReduxState } from '../../../../types';
import BreadCrumb from '../../../Common/Layout/BreadCrumb/BreadCrumb';
import CreateDatabase from './CreateDatabase';
import { DataSource } from '../../../../metadata/types';
import { Driver } from '../../../../dataSources';
import {
  removeDataSource,
  reloadDataSource,
  addDataSource,
} from '../../../../metadata/actions';
import { RightContainer } from '../../../Common/Layout/RightContainer';
import { getDataSources } from '../../../../metadata/selector';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import ToolTip from '../../../Common/Tooltip/Tooltip';

type DatabaseListItemProps = {
  dataSource: DataSource;
  onReload: (name: string, driver: Driver, cb: () => void) => void;
  onRemove: (name: string, driver: Driver, cb: () => void) => void;
};
const DatabaseListItem: React.FC<DatabaseListItemProps> = ({
  onReload,
  onRemove,
  dataSource,
}) => {
  const [reloading, setReloading] = useState(false);
  const [removing, setRemoving] = useState(false);
  const [showUrl, setShowUrl] = useState(dataSource.name === 'default');

  return (
    <div className={styles.db_list_item}>
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
        className={styles.db_list_content}
        size="xs"
        color="white"
        onClick={() => {
          setRemoving(true);
          onRemove(dataSource.name, dataSource.driver, () =>
            setRemoving(false)
          );
        }}
      >
        {removing ? 'Removing...' : 'Remove'}
      </Button>
      <div className={styles.db_list_content}>
        <b>
          {dataSource.name} ({dataSource.driver})
        </b>{' '}
        -
        <span style={{ paddingLeft: 5 }}>
          {showUrl ? (
            dataSource.url
          ) : (
            <ToolTip
              id="connection-string-show"
              placement="right"
              message="Show connection string"
            >
              <i
                className={`${styles.showAdminSecret} fa fa-eye`}
                aria-hidden="true"
                onClick={() => setShowUrl(true)}
              />
            </ToolTip>
          )}
          {showUrl && dataSource.name !== 'default' && (
            <ToolTip
              id="connection-string-hide"
              placement="right"
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
        </span>
      </div>
    </div>
  );
};

const crumbs = [
  {
    title: 'Data',
    url: '/data',
  },
  {
    title: 'Manage',
    url: '#',
  },
];

const ManageDatabase: React.FC<ManageDatabaseInjectedProps> = ({
  dataSources,
  dispatch,
}) => {
  const onRemove = (name: string, driver: Driver, cb: () => void) => {
    const confirmation = getConfirmation(
      `Your action will remove the "${name}" data source`,
      true,
      name
    );
    if (confirmation) {
      (dispatch(removeDataSource({ driver, name })) as any).then(cb); // todo
    }
  };

  const onReload = (name: string, driver: Driver, cb: () => void) => {
    (dispatch(reloadDataSource({ driver, name })) as any).then(cb); // todo
  };

  const onCreateDataSource = (
    data: DataSource,
    successCallback: () => void
  ) => {
    dispatch(
      addDataSource(
        {
          driver: data.driver,
          payload: {
            name: data.name.trim(),
            connection_pool_setting: {
              ...(data.connection_pool_setting?.idle_timeout && {
                idle_timeout: data.connection_pool_setting.idle_timeout,
              }),
              ...(data.connection_pool_setting?.max_connections && {
                max_connections: data.connection_pool_setting.max_connections,
              }),
              ...(data.connection_pool_setting?.retries && {
                max_connections: data.connection_pool_setting.retries,
              }),
            },
            dbUrl: data.url,
          },
        },
        successCallback
      )
    );
  };

  return (
    <RightContainer>
      <div className={`container-fluid ${styles.manage_dbs_page}`}>
        <Helmet title="Manage - Data | Hasura" />
        <BreadCrumb breadCrumbs={crumbs} />
        <div className={styles.display_flex}>
          <h2
            className={`${styles.headerText} ${styles.display_inline} ${styles.padd_top}`}
          >
            Manage Databases
          </h2>
          {/* <Button color="yellow" size="md" className={styles.add_mar_left}>
            Add Database
          </Button> */}
        </div>
        <div className={styles.manage_db_content}>
          <hr />
          <h3 className={`${styles.heading_text} ${styles.remove_pad_bottom}`}>
            Databases
          </h3>
          <div className={styles.data_list_container}>
            {dataSources.length ? (
              dataSources.map(data => (
                <DatabaseListItem
                  key={data.name}
                  dataSource={data}
                  onReload={onReload}
                  onRemove={onRemove}
                />
              ))
            ) : (
              <span style={{ paddingTop: 15 }}>
                You don&apos;t have any data sources
              </span>
            )}
          </div>
          <hr />
        </div>
        <CreateDatabase onSubmit={onCreateDataSource} />
      </div>
    </RightContainer>
  );
};

const mapStateToProps = (state: ReduxState) => {
  return {
    schemaList: state.tables.schemaList,
    dataSources: getDataSources(state),
  };
};
const manageConnector = connect(
  mapStateToProps,
  (dispatch: ThunkDispatch<ReduxState, unknown, any>) => ({
    dispatch,
  })
);

type ManageDatabaseInjectedProps = ConnectedProps<typeof manageConnector>;

const ConnectedDatabaseManagePage = manageConnector(ManageDatabase);
export default ConnectedDatabaseManagePage;
