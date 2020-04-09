import PropTypes from 'prop-types';
import React, { Component } from 'react';
import { connect } from 'react-redux';
import ProgressBar from 'react-progress-bar-plus';
import Notifications from 'react-notification-system-redux';
import { hot } from 'react-hot-loader';
import { ThemeProvider } from 'styled-components';

import ErrorBoundary from '../Error/ErrorBoundary';
import {
  loadConsoleOpts,
  telemetryNotificationShown,
} from '../../telemetry/Actions';
import { showTelemetryNotification } from '../../telemetry/Notifications';

import { theme } from '../UIKit/theme';

class App extends Component {
  componentDidMount() {
    const { dispatch } = this.props;

    // Hide the loader once the react component is ready.
    // NOTE: This will execute only once (since this is the parent component for all other components).
    const className = document.getElementById('content').className;
    document.getElementById('content').className = className + ' show';
    document.getElementById('loading').style.display = 'none';

    dispatch(loadConsoleOpts());
  }

  componentDidUpdate() {
    const { telemetry, dispatch } = this.props;
    if (
      telemetry.console_opts &&
      !telemetry.console_opts.telemetryNotificationShown
    ) {
      dispatch(telemetryNotificationShown());
      dispatch(showTelemetryNotification());
    }
  }

  render() {
    const styles = require('./App.scss');
    const {
      requestError,
      error,
      ongoingRequest,
      percent,
      intervalTime,
      children,
      notifications,
      connectionFailed,
      dispatch,
      metadata,
    } = this.props;

    if (requestError && error) {
      // console.error(requestError, error);
    }

    let connectionFailMsg = null;
    if (connectionFailed) {
      connectionFailMsg = (
        <div
          style={{ marginBottom: '0px' }}
          className={styles.alertDanger + ' alert alert-danger'}
        >
          <strong>
            Hasura console is not able to reach your Hasura GraphQL engine
            instance. Please ensure that your instance is running and the
            endpoint is configured correctly.
          </strong>
        </div>
      );
    }

    return (
      <ThemeProvider theme={theme}>
        <ErrorBoundary metadata={metadata} dispatch={dispatch}>
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
            <Notifications notifications={notifications} />
          </div>
        </ErrorBoundary>
      </ThemeProvider>
    );
  }
}

App.propTypes = {
  reqURL: PropTypes.string,
  reqData: PropTypes.object,
  statusCode: PropTypes.number,

  error: PropTypes.object,
  ongoingRequest: PropTypes.bool,
  requestError: PropTypes.bool,
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
    telemetry: state.telemetry,
    metadata: state.metadata,
  };
};

export default hot(module)(connect(mapStateToProps)(App));
