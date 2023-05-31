import PropTypes from 'prop-types';
import React from 'react';
import { connect } from 'react-redux';
import ProgressBar from 'react-progress-bar-plus';
import { hot } from 'react-hot-loader';
import { ThemeProvider } from 'styled-components';
import 'react-loading-skeleton/dist/skeleton.css';
import ErrorBoundary from '../Error/ErrorBoundary';
import globals from '../../Globals';
import styles from './App.module.scss';
import { ToastsHub } from '../../new-components/Toasts';
import { AlertProvider } from '../../new-components/Alert/AlertProvider';

import { theme } from '../UIKit/theme';
import { trackCustomEvent } from '../../features/Analytics';
import { withLDProvider } from 'launchdarkly-react-client-sdk';
import { isCloudConsole } from '../../utils';

export const GlobalContext = React.createContext(globals);

const App = ({
  ongoingRequest,
  percent,
  intervalTime,
  children,
  connectionFailed,
  dispatch,
  metadata,
}) => {
  React.useEffect(() => {
    const className = document.getElementById('content').className;
    document.getElementById('content').className = className + ' show';
    document.getElementById('loading').style.display = 'none';
    try {
      document.getElementsByClassName('loadingWrapper')[0].style.display =
        'none';
    } catch (e) {
      console.error('Could not find loadingWrapper', e);
    }
    trackCustomEvent({
      location: 'Console',
      action: 'Load',
      object: 'App',
    });
  }, []);

  let connectionFailMsg = null;
  if (connectionFailed) {
    connectionFailMsg = (
      <div
        className={`${styles.alertDanger} ${styles.remove_margin_bottom} alert alert-danger `}
      >
        <strong>
          Hasura console is not able to reach your Hasura GraphQL engine
          instance. Please ensure that your instance is running and the endpoint
          is configured correctly.
        </strong>
      </div>
    );
  }

  return (
    <GlobalContext.Provider value={globals}>
      <ThemeProvider theme={theme}>
        <ErrorBoundary metadata={metadata} dispatch={dispatch}>
          <AlertProvider>
            <div>
              {connectionFailMsg}
              {ongoingRequest && (
                <ProgressBar
                  percent={percent}
                  autoIncrement={true} // eslint-disable-line react/jsx-boolean-value
                  intervalTime={intervalTime}
                  spinner={false}
                />
              )}
              <div>{children}</div>
              <ToastsHub />
            </div>
          </AlertProvider>
        </ErrorBoundary>
      </ThemeProvider>
    </GlobalContext.Provider>
  );
};

App.propTypes = {
  reqURL: PropTypes.string,
  reqData: PropTypes.object,
  statusCode: PropTypes.number,

  ongoingRequest: PropTypes.bool,
  connectionFailed: PropTypes.bool,

  intervalTime: PropTypes.number,
  percent: PropTypes.number,

  children: PropTypes.element,
  dispatch: PropTypes.func.isRequired,

  notifications: PropTypes.array,
};

const mapStateToProps = state => {
  return {
    ...state.progressBar,
    notifications: state.notifications,
    metadata: state.metadata,
  };
};

const LAUNCHDARKLY_CLIENT_ID = globals.launchDarklyClientId;

export default isCloudConsole(globals)
  ? withLDProvider({
      // initialize with the dev key if env is not production
      clientSideID: LAUNCHDARKLY_CLIENT_ID,
      options: {
        diagnosticOptOut: true,
        // allow settting up a streaming connection
        streaming: true,
        // information to help track events from this app on LD
        application: {
          id: 'hasura-cloud-console',
        },
      },
      reactOptions: {
        sendEventsOnFlagRead: true,
        useCamelCaseFlagKeys: false,
      },
    })(hot(module)(connect(mapStateToProps)(App)))
  : hot(module)(connect(mapStateToProps)(App));
