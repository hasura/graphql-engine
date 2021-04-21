import * as React from 'react';
import styles from '../styles.scss';
import { HerokuSession } from './types';
import {
  useHerokuDBCreation,
  setDBURLInEnvVars,
  verifyProjectHealthAndConnectDataSource,
  startHerokuDBURLSync,
} from './utils';
import DBCreationStatus from './DBCreationStatus';
import { Dispatch } from '../../../../../../types';
import { setDBConnectionDetails } from '../../../DataActions';
import _push from '../../../push';
import {
  connectDataSource,
  connectionTypes,
  getDefaultState,
} from '../../../DataSources/state';
import HerokuLogoComponent from './HerokuButtonLogo';
import { DataSource } from '../../../../../../metadata/types';

type Props = {
  session: HerokuSession;
  shouldStart: boolean;
  dispatch: Dispatch;
  allDataSources: DataSource[];
};

const DBCreation: React.FC<Props> = ({
  session,
  shouldStart,
  dispatch,
  allDataSources,
}) => {
  const { start, state, inProgress } = useHerokuDBCreation(
    session,
    shouldStart,
    dispatch
  );
  const [isSettingEnvVar, setIsSettingEnvVar] = React.useState(false);
  const [createdEnvVar, setCreatedEnvVar] = React.useState('');
  const [isConnectingDataSource, setIsConnectingDataSource] = React.useState(
    false
  );
  const loading = inProgress || isSettingEnvVar || isConnectingDataSource;
  const herokuButtonClassName = loading
    ? `${styles.herokuButtonBoxDisabled} ${styles.add_mar_bottom}`
    : `${styles.herokuButtonBox} ${styles.add_mar_bottom}`;

  React.useEffect(() => {
    // TODO move this to utils
    if (
      state['getting-config'].status === 'success' &&
      state['creating-app'].status === 'success'
    ) {
      const dbURL = state['getting-config'].details.DATABASE_URL;
      const appName = state['creating-app'].details.name;
      const appID = state['creating-app'].details.id;
      const dbName = allDataSources.length ? `herokuapp-${appName}` : 'default';
      setIsSettingEnvVar(true);
      setDBURLInEnvVars(dbURL)
        .then(envVar => {
          setIsSettingEnvVar(false);
          setCreatedEnvVar(envVar);
          dispatch(
            setDBConnectionDetails({
              envVar,
              dbName,
            })
          );
          setIsConnectingDataSource(true);
          const connectEnvVarDataSource = () => {
            connectDataSource(
              dispatch,
              connectionTypes.ENV_VAR,
              getDefaultState({
                dbConnection: {
                  envVar,
                  dbName,
                },
              }),
              () => {
                startHerokuDBURLSync(envVar, appName, appID);
                dispatch(setDBConnectionDetails({}));
                dispatch(_push('/data/manage'));
              }
            );
          };
          const pushToConnect = () => {
            dispatch(_push('/data/manage/connect'));
          };
          setTimeout(() => {
            verifyProjectHealthAndConnectDataSource(
              connectEnvVarDataSource,
              pushToConnect
            );
          }, 9000);
        })
        .catch(e => {
          console.error(e);
          if (isSettingEnvVar) {
            dispatch(
              setDBConnectionDetails({
                dbURL,
                dbName,
              })
            );
          } else {
            dispatch(
              setDBConnectionDetails({
                envVar: createdEnvVar,
                dbName,
              })
            );
          }
          dispatch(_push('/data/manage/connect'));
        });
    }
  }, [state['getting-config']]);

  let statusText = 'Creating database';
  if (isSettingEnvVar) {
    statusText = 'Setting database URL in env vars';
  }
  if (isConnectingDataSource) {
    statusText = 'Connecting to Hasura';
  }

  return (
    <div
      className={`${styles.wd100Percent} ${styles.display_flex} ${styles.flexColumn}`}
    >
      <div
        className={herokuButtonClassName}
        onClick={() => {
          if (loading) {
            return;
          }
          start(session);
        }}
      >
        <HerokuLogoComponent />
      </div>
      <div className={`${styles.display_flex} ${styles.add_mar_bottom_mid}`}>
        <p>
          <i className="fa fa-circle-o-notch fa-spin" aria-hidden="true" />
          &nbsp;{statusText}
        </p>
      </div>
      <DBCreationStatus state={state} />
    </div>
  );
};

export default DBCreation;
