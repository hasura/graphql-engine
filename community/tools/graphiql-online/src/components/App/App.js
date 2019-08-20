import PropTypes from 'prop-types';
import React, { Component } from 'react';
import { connect } from 'react-redux';
import ProgressBar from 'react-progress-bar-plus';
import { hot } from 'react-hot-loader';
import ErrorBoundary from '../Error/ErrorBoundary';

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
      connectionFailed,
      dispatch,
    } = this.props;

    return (
      <ErrorBoundary dispatch={dispatch}>
        <div>
          {ongoingRequest && (
            <ProgressBar
              percent={percent}
              autoIncrement={true} // eslint-disable-line react/jsx-boolean-value
              intervalTime={intervalTime}
              spinner={false}
            />
          )}
          <div>{children}</div>
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

};

const mapStateToProps = state => {
  return {
    ...state.progressBar,
  };
};

export default hot(module)(connect(mapStateToProps)(App));
