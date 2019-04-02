import React from 'react';
import PropTypes from 'prop-types';
import { loadInconsistentObjects } from '../Services/Data/Metadata/Actions';
import Spinner from '../Common/Spinner/Spinner';
import { push } from 'react-router-redux';
import globals from '../../Globals';

class ErrorBoundary extends React.Component {
  constructor(props) {
    super(props);
    this.state = { hasError: false, info: null };
  }

  componentDidCatch(error, info) {
    this.setState({ hasError: true, info: info });
    const { dispatch } = this.props;
    dispatch(loadInconsistentObjects(null, true)).then(() => {
      if (this.props.metadata.inconsistentObjects.length > 0) {
        this.setState({ hasError: false, info: null });
        this.props.dispatch(push(globals.urlPrefix + '/metadata'));
      } else {
        console.error(error);
      }
    });
  }

  render() {
    const { metadata } = this.props;
    if (this.state.hasError && metadata.ongoingRequest) {
      return (
        <div>
          {' '}
          <Spinner />{' '}
        </div>
      );
    }
    if (this.state.hasError) {
      // You can render any custom fallback UI
      return <div>Something went wrong</div>;
    }
    return this.props.children;
  }
}

ErrorBoundary.propTypes = {
  children: PropTypes.element,
};

export default ErrorBoundary;
