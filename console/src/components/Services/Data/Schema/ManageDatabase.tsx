import React, { useState } from 'react';
import Helmet from 'react-helmet';
import { connect, ConnectedProps } from 'react-redux';

import Button from '../../../Common/Button/Button';
import styles from '../../../Common/Common.scss';
import { ReduxState } from '../../../../types';
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
import { getHostFromConnectionString } from '../DataSources/ManageDBUtils';
import { isInconsistentSource } from '../utils';

const driverToLabel: Record<Driver, string> = {
  mysql: 'MySQL',
  postgres: 'PostgreSQL',
  mssql: 'MS Server',
};

type DatabaseListItemProps = {
  dataSource: DataSource;
  inconsistentObjects: InjectedProps['inconsistentObjects'];
  // onEdit: (dbName: string) => void;
  onReload: (name: string, driver: Driver, cb: () => void) => void;
  onRemove: (name: string, driver: Driver, cb: () => void) => void;
};

const DatabaseListItem: React.FC<DatabaseListItemProps> = ({
  // onEdit,
  onReload,
  onRemove,
  dataSource,
  inconsistentObjects,
}) => {
  const [reloading, setReloading] = useState(false);
  const [removing, setRemoving] = useState(false);
  const [showUrl, setShowUrl] = useState(false);

  return (
    <div className={styles.db_list_item}>
      <div className={styles.db_item_actions}>
        {/* Edit shall be cut out until we have a server API 
        <Button
          size="xs"
          color="white"
          style={{ marginRight: '10px' }}
          onClick={() => onEdit(dataSource.name)}
        >
          Edit
        </Button> */}
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
      </div>
      <div className={styles.db_list_content}>
        <div className={styles.db_display_data}>
          <div className={styles.displayFlexContainer}>
            <b>{dataSource.name}</b>&nbsp;
            <p>({driverToLabel[dataSource.driver]})</p>
          </div>
          <p style={{ marginTop: -5 }}>
            {getHostFromConnectionString(dataSource)}
          </p>
        </div>
        {isInconsistentSource(dataSource.name, inconsistentObjects) && (
          <ToolTip
            id={`inconsistent-source-${dataSource.name}`}
            placement="right"
            message="Inconsistent Data Source"
          >
            <i
              className="fa fa-exclamation-triangle"
              aria-hidden="true"
              style={{ padding: 3, color: '#c02020' }}
            />
          </ToolTip>
        )}
      </div>
      <span style={{ paddingLeft: 125 }}>
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
  );
};

interface ManageDatabaseProps extends InjectedProps {}

const ManageDatabase: React.FC<ManageDatabaseProps> = ({
  dataSources,
  dispatch,
  inconsistentObjects,
}) => {
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

  // const onEdit = (dbName: string) => {
  //   dispatch(_push(`/data/manage/edit/${dbName}`));
  // };

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
              Manage Databases
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
          <hr />
          <h3 className={`${styles.heading_text} ${styles.remove_pad_bottom}`}>
            Connected Databases
          </h3>
          <div className={styles.data_list_container}>
            {dataSources.length ? (
              dataSources.map(data => (
                <DatabaseListItem
                  key={data.name}
                  dataSource={data}
                  inconsistentObjects={inconsistentObjects}
                  // onEdit={onEdit}
                  onReload={onReload}
                  onRemove={onRemove}
                />
              ))
            ) : (
              <span style={{ paddingTop: 15 }}>
                You don&apos;t have any data sources connected.
              </span>
            )}
          </div>
          <hr />
        </div>
      </div>
    </RightContainer>
  );
};

const mapStateToProps = (state: ReduxState) => {
  return {
    schemaList: state.tables.schemaList,
    dataSources: getDataSources(state),
    currentDataSource: state.tables.currentDataSource,
    currentSchema: state.tables.currentSchema,
    inconsistentObjects: state.metadata.inconsistentObjects,
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);
type InjectedProps = ConnectedProps<typeof connector>;
const ConnectedDatabaseManagePage = connector(ManageDatabase);
export default ConnectedDatabaseManagePage;
