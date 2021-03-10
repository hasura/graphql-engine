import React, { ChangeEvent } from 'react';
import { connect, ConnectedProps } from 'react-redux';

import Tabbed from './TabbedDataSourceConnection';
import { ReduxState } from '../../../../types';
import { mapDispatchToPropsEmpty } from '../../../Common/utils/reactUtils';
import Button from '../../../Common/Button';
import { showErrorNotification } from '../../Common/Notification';
import _push from '../push';

import styles from '../../../Common/Common.scss';
import {
  connectDataSource,
  connectDBReducer,
  connectionTypes,
  getDefaultState,
} from './state';
import { getDatasourceURL, getErrorMessageFromMissingFields } from './utils';
import { LabeledInput } from '../../../Common/LabeledInput';
import { Driver } from '../../../../dataSources';

interface ConnectDatabaseProps extends InjectedProps {}

const connectionRadioName = 'connection-type';
const dbTypePlaceholders: Record<Driver, string> = {
  postgres: 'postgresql://username:password@hostname:5432/database',
  mssql:
    'Driver={ODBC Driver 17 for SQL Server};Server=serveraddress;Database=dbname;Uid=username;Pwd=password;',
  mysql: 'MySQL connection string',
};

const connectionRadios = [
  {
    value: connectionTypes.CONNECTION_PARAMS,
    title: 'Connection Parameters',
    disableOnEdit: true,
  },
  {
    value: connectionTypes.DATABASE_URL,
    title: 'Database URL',
    disableOnEdit: false,
  },
  {
    value: connectionTypes.ENV_VAR,
    title: 'Environment Variable',
    disableOnEdit: true,
  },
];

const ConnectDatabase: React.FC<ConnectDatabaseProps> = props => {
  const { dispatch } = props;

  const [connectDBInputState, connectDBDispatch] = React.useReducer(
    connectDBReducer,
    getDefaultState(props)
  );

  const [connectionType, changeConnectionType] = React.useState(
    props.dbConnection.envVar
      ? connectionTypes.ENV_VAR
      : connectionTypes.DATABASE_URL
  );
  const [openConnectionSettings, changeConnectionsParamState] = React.useState(
    false
  );
  const [loading, setLoading] = React.useState(false);
  const { sources = [], pathname = '' } = props;

  const isEditState =
    pathname.includes('edit') || pathname.indexOf('edit') !== -1;
  const paths = pathname.split('/');
  const editSourceName = paths[paths.length - 1];
  const currentSourceInfo = sources.find(
    source => source.name === editSourceName
  );
  React.useEffect(() => {
    if (isEditState && currentSourceInfo) {
      connectDBDispatch({
        type: 'INIT',
        data: {
          name: currentSourceInfo.name,
          driver: currentSourceInfo.kind ?? 'postgres',
          databaseUrl: getDatasourceURL(
            currentSourceInfo?.configuration?.connection_info?.database_url
          ),
          connectionSettings:
            currentSourceInfo.configuration?.connection_info?.pool_settings ??
            {},
        },
      });
    }
  }, [isEditState, currentSourceInfo]);

  const onChangeConnectionType = (e: ChangeEvent<HTMLInputElement>) => {
    changeConnectionType(e.target.value);
  };

  const resetState = () => {
    connectDBDispatch({
      type: 'RESET_INPUT_STATE',
    });
    dispatch(_push('/data/manage'));
  };

  const onSuccessConnectDBCb = () => {
    resetState();
    // route to manage page
    dispatch(_push('/data/manage'));
  };

  const onClickConnectDatabase = () => {
    if (!connectDBInputState.displayName.trim()) {
      dispatch(
        showErrorNotification(
          'Display Name is a mandatory field',
          'Please enter a valid display name'
        )
      );
      return;
    }

    if (isEditState) {
      // TODO: server to provide API
    }

    if (
      connectionType === connectionTypes.DATABASE_URL ||
      (connectionType === connectionTypes.CONNECTION_PARAMS &&
        connectDBInputState.dbType === 'mssql')
    ) {
      if (!connectDBInputState.databaseURLState.dbURL.trim()) {
        dispatch(
          showErrorNotification(
            'Database URL is a mandatory field',
            'Please enter a valid database URL'
          )
        );
        return;
      }

      setLoading(true);
      connectDataSource(
        dispatch,
        connectionTypes.DATABASE_URL,
        connectDBInputState,
        onSuccessConnectDBCb
      )
        .then(() => setLoading(false))
        .catch(() => setLoading(false));
      return;
    }

    if (connectionType === connectionTypes.ENV_VAR) {
      if (!connectDBInputState.envVarURLState.envVarURL.trim()) {
        dispatch(
          showErrorNotification(
            'Environment Variable is a mandatory field',
            'Please enter the name of a valid environment variable'
          )
        );
        return;
      }

      setLoading(true);
      connectDataSource(
        dispatch,
        connectionType,
        connectDBInputState,
        onSuccessConnectDBCb
      )
        .then(() => setLoading(false))
        .catch(() => setLoading(false));
      return;
    }

    // construct the connection string from connection params and
    // make the same call as done for connection of type DATABASE_URL
    const {
      host,
      port,
      username,
      database,
    } = connectDBInputState.connectionParamState;

    if (!host || !port || !username || !database) {
      const errorMessage = getErrorMessageFromMissingFields(
        host,
        port,
        username,
        database
      );
      dispatch(
        showErrorNotification('Required fields are missing', errorMessage)
      );
      return;
    }
    setLoading(true);
    connectDataSource(
      dispatch,
      connectionType,
      connectDBInputState,
      onSuccessConnectDBCb
    )
      .then(() => setLoading(false))
      .catch(() => setLoading(false));
  };

  return (
    <Tabbed tabName="connect">
      <div className={styles.connect_db_content}>
        <h4
          className={`${styles.remove_pad_bottom} ${styles.connect_db_header}`}
        >
          Connect Database Via
        </h4>
        <div
          className={styles.connect_db_radios}
          onChange={onChangeConnectionType}
        >
          {connectionRadios.map(
            (radioBtn: {
              value: string;
              title: string;
              disableOnEdit: boolean;
            }) => (
              <label className={styles.connect_db_radio_label}>
                <input
                  type="radio"
                  value={radioBtn.value}
                  name={connectionRadioName}
                  checked={connectionType === radioBtn.value}
                  disabled={
                    isEditState === radioBtn.disableOnEdit && isEditState
                  }
                />
                {radioBtn.title}
              </label>
            )
          )}
        </div>
        <div className={styles.connect_form_layout}>
          <LabeledInput
            onChange={e =>
              connectDBDispatch({
                type: 'UPDATE_DISPLAY_NAME',
                data: e.target.value,
              })
            }
            value={connectDBInputState.displayName}
            label="Database Display Name"
            placeholder="database name"
          />
          <label className={styles.connect_db_input_label}>
            Data Source Driver
          </label>
          <select
            key="connect-db-type"
            value={connectDBInputState.dbType}
            onChange={e =>
              connectDBDispatch({
                type: 'UPDATE_DB_DRIVER',
                data: e.target.value as Driver,
              })
            }
            className={`form-control ${styles.connect_db_input_pad}`}
          >
            <option key="postgres" value="postgres">
              Postgres
            </option>
            <option key="mssql" value="mssql">
              MS Server
            </option>
          </select>
          {connectionType === connectionTypes.DATABASE_URL ||
          (connectionType === connectionTypes.CONNECTION_PARAMS &&
            connectDBInputState.dbType === 'mssql') ? (
            <LabeledInput
              label="Database URL"
              onChange={e =>
                connectDBDispatch({
                  type: 'UPDATE_DB_URL',
                  data: e.target.value,
                })
              }
              value={connectDBInputState.databaseURLState.dbURL}
              placeholder={dbTypePlaceholders[connectDBInputState.dbType]}
              disabled={isEditState}
            />
          ) : null}
          {connectionType === connectionTypes.ENV_VAR ? (
            <LabeledInput
              label="Environment Variable"
              placeholder="DB_URL_FROM_ENV"
              onChange={e =>
                connectDBDispatch({
                  type: 'UPDATE_DB_URL_ENV_VAR',
                  data: e.target.value,
                })
              }
              value={connectDBInputState.envVarURLState.envVarURL}
            />
          ) : null}
          {connectionType === connectionTypes.CONNECTION_PARAMS &&
          connectDBInputState.dbType !== 'mssql' ? (
            <>
              <LabeledInput
                label="Host"
                placeholder="localhost"
                onChange={e =>
                  connectDBDispatch({
                    type: 'UPDATE_DB_HOST',
                    data: e.target.value,
                  })
                }
                value={connectDBInputState.connectionParamState.host}
              />
              <LabeledInput
                label="Port"
                placeholder="5432"
                onChange={e =>
                  connectDBDispatch({
                    type: 'UPDATE_DB_PORT',
                    data: e.target.value,
                  })
                }
                value={connectDBInputState.connectionParamState.port}
              />
              <LabeledInput
                label="Username"
                placeholder="postgres_user"
                onChange={e =>
                  connectDBDispatch({
                    type: 'UPDATE_DB_USERNAME',
                    data: e.target.value,
                  })
                }
                value={connectDBInputState.connectionParamState.username}
              />
              <LabeledInput
                label="Password"
                key="connect-db-password"
                type="password"
                placeholder="postgrespassword"
                onChange={e =>
                  connectDBDispatch({
                    type: 'UPDATE_DB_PASSWORD',
                    data: e.target.value,
                  })
                }
                value={connectDBInputState.connectionParamState.password}
              />
              <LabeledInput
                key="connect-db-database-name"
                label="Database Name"
                placeholder="postgres"
                onChange={e =>
                  connectDBDispatch({
                    type: 'UPDATE_DB_DATABASE_NAME',
                    data: e.target.value,
                  })
                }
                value={connectDBInputState.connectionParamState.database}
              />
            </>
          ) : null}
          <div className={styles.connection_settings_layout}>
            <div className={styles.connection_settings_header}>
              <a
                href="#"
                style={{ textDecoration: 'none' }}
                onClick={() =>
                  changeConnectionsParamState(!openConnectionSettings)
                }
              >
                {openConnectionSettings ? (
                  <i className="fa fa-caret-down" />
                ) : (
                  <i className="fa fa-caret-right" />
                )}
                {'  '}
                Connection Settings
              </a>
            </div>
            {openConnectionSettings ? (
              <div className={styles.connection_settings_form}>
                <div className={styles.connnection_settings_form_input_layout}>
                  <LabeledInput
                    label="Max Connections"
                    type="number"
                    className={`form-control ${styles.connnection_settings_form_input}`}
                    placeholder="50"
                    value={
                      connectDBInputState.connectionSettings?.max_connections ??
                      undefined
                    }
                    onChange={e =>
                      connectDBDispatch({
                        type: 'UPDATE_MAX_CONNECTIONS',
                        data: e.target.value,
                      })
                    }
                    min="0"
                    labelInBold
                  />
                </div>
                <div className={styles.connnection_settings_form_input_layout}>
                  <LabeledInput
                    label="Idle Timeout"
                    type="number"
                    className={`form-control ${styles.connnection_settings_form_input}`}
                    placeholder="180"
                    value={
                      connectDBInputState.connectionSettings?.idle_timeout ??
                      undefined
                    }
                    onChange={e =>
                      connectDBDispatch({
                        type: 'UPDATE_IDLE_TIMEOUT',
                        data: e.target.value,
                      })
                    }
                    min="0"
                    labelInBold
                  />
                </div>
                <div className={styles.connnection_settings_form_input_layout}>
                  <LabeledInput
                    label="Retries"
                    type="number"
                    className={`form-control ${styles.connnection_settings_form_input}`}
                    placeholder="1"
                    value={
                      connectDBInputState.connectionSettings?.retries ??
                      undefined
                    }
                    onChange={e =>
                      connectDBDispatch({
                        type: 'UPDATE_RETRIES',
                        data: e.target.value,
                      })
                    }
                    min="0"
                    labelInBold
                  />
                </div>
              </div>
            ) : null}
          </div>
          <div className={styles.add_button_layout}>
            <Button
              onClick={onClickConnectDatabase}
              size="large"
              color="yellow"
              style={{
                width: '70%',
                ...(loading && { cursor: 'progress' }),
              }}
              disabled={loading}
            >
              {!isEditState ? 'Connect Database' : 'Edit Connection'}
            </Button>
          </div>
        </div>
      </div>
    </Tabbed>
  );
};

const mapStateToProps = (state: ReduxState) => {
  return {
    currentDataSource: state.tables.currentDataSource,
    currentSchema: state.tables.currentSchema,
    sources: state.metadata.metadataObject?.sources ?? [],
    dbConnection: state.tables.dbConnection,
    pathname: state?.routing?.locationBeforeTransitions?.pathname,
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);
type InjectedProps = ConnectedProps<typeof connector>;
const ConnectedDatabaseConnectPage = connector(ConnectDatabase);
export default ConnectedDatabaseConnectPage;
