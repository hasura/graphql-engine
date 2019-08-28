import React from 'react';
import PropTypes from 'prop-types';
import {
  loadInconsistentObjects,
  redirectToMetadataStatus,
  isMetadataStatusPage,
} from '../Services/Metadata/Actions';
import Spinner from '../Common/Spinner/Spinner';

import PageNotFound, { NotFoundError } from './PageNotFound';
import RuntimeError from './RuntimeError';

class ErrorBoundary extends React.Component {
  initialState = {
    hasError: false,
    info: null,
    error: null,
    type: '500',
  };

  constructor(props) {
    super(props);

    this.state = this.initialState;
  }

  resetState = () => {
    this.setState({ ...this.initialState });
  };

  componentDidCatch(error, info) {
    const { dispatch } = this.props;

    // for invalid path segment errors
    if (error instanceof NotFoundError) {
      this.setState({
        type: '404',
      });
    }

    this.setState({ hasError: true, info: info, error: error });

    dispatch(loadInconsistentObjects(true)).then(() => {
      if (this.props.metadata.inconsistentObjects.length > 0) {
        if (!isMetadataStatusPage()) {
          this.resetState();
          this.props.dispatch(redirectToMetadataStatus());
        }
      } else {
        console.error(error);
      }
    });
  }

  render() {
    const { metadata } = this.props;
    const { hasError, type, error } = this.state;

    if (hasError && metadata.ongoingRequest) {
      return (
        <div>
          <Spinner />
        </div>
      );
    }

    if (hasError) {
      return type === '404' ? (
        <PageNotFound resetCallback={this.resetState} />
      ) : (
        <RuntimeError resetCallback={this.resetState} error={error} />
      );
    }

    return this.props.children;
  }
}

ErrorBoundary.propTypes = {
  children: PropTypes.element,
};

export default ErrorBoundary;
