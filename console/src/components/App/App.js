import PropTypes from 'prop-types';
import React, { Component } from 'react';
import { connect } from 'react-redux';
import ProgressBar from 'react-progress-bar-plus';
import Notifications from 'react-notification-system-redux';
import { hot } from 'react-hot-loader';
import ErrorBoundary from '../Error/ErrorBoundary';
import { telemetryNotificationShown } from '../../telemetry/Actions';
import { showTelemetryNotification } from '../../telemetry/Notifications';

class App extends Component {
  componentDidMount() {
    // Hide the loader once the react component is ready.
    // NOTE: This will execute only onces (since this is parent component for all other component).
    const className = document.getElementById('content').className;
    document.getElementById('content').className = className + ' show';
    document.getElementById('loading').style.display = 'none';
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
      telemetry,
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

    if (telemetry.console_opts) {
      if (!telemetry.console_opts.telemetryNotificationShown) {
        dispatch(showTelemetryNotification());
        dispatch(telemetryNotificationShown());
      }
    }

    return (
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
