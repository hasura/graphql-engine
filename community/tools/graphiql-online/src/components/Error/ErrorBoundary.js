import React from 'react';
import PropTypes from 'prop-types';
import Spinner from '../Common/Spinner/Spinner';

import PageNotFound, { NotFoundError } from './PageNotFound';

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
      <PageNotFound resetCallback={this.resetState} />
    }

    return this.props.children;
  }
}

ErrorBoundary.propTypes = {
  children: PropTypes.element,
};

export default ErrorBoundary;
