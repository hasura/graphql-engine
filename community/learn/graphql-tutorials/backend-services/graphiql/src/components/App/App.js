import PropTypes from 'prop-types';
import React, { Component } from 'react';
import { connect } from 'react-redux';
import ProgressBar from 'react-progress-bar-plus';
import './progress-bar.scss';
import { NOTIF_EXPANDED } from './Actions';
import ErrorBoundary from './ErrorBoundary';

class App extends Component {
  componentDidMount() {
    // Hide the loader once the react component is ready.
    // NOTE: This will execute only onces (since this is parent component for all other component).
    const className = document.getElementById('content').className;
    document.getElementById('content').className = className + ' show';
    document.getElementById('loading').style.display = 'none';
  }

  onModalClose = () => {
    this.props.dispatch({ type: NOTIF_EXPANDED, data: false });
  };

  render() {
    const {
      ongoingRequest,
      percent,
      intervalTime,
      children,
    } = this.props;

    return (
      <ErrorBoundary>
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

  modalOpen: PropTypes.bool,
  error: PropTypes.object,
  ongoingRequest: PropTypes.bool,
  requestError: PropTypes.bool,
  connectionFailed: PropTypes.bool,

  intervalTime: PropTypes.number,
  percent: PropTypes.number,

  children: PropTypes.element,
  dispatch: PropTypes.func.isRequired,

  isNotifExpanded: PropTypes.bool,
  notifMsg: PropTypes.string,
};

const mapStateToProps = state => {
  return {
    ...state.progressBar,
  };
};

export default connect(mapStateToProps)(App);
