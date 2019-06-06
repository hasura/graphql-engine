import React from 'react';
import PropTypes from 'prop-types';
import {
  loadInconsistentObjects,
  redirectToMetadataStatus,
  isMetadataStatusPage,
} from '../Services/Metadata/Actions';
import Spinner from '../Common/Spinner/Spinner';

import { Link } from 'react-router';
import Helmet from 'react-helmet';

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
    const errorImage = require('./error-logo.png');
    const styles = require('./ErrorPage.scss');
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
      if (this.state.type === '404') {
        return <PageNotFound />;
      }

      return (
        <div className={styles.viewContainer}>
          <Helmet title="Error | Hasura" />
          <div className={'container ' + styles.centerContent}>
            <div className={'row ' + styles.message}>
              <div className="col-xs-8">
                <h1>Error</h1>
                <br />
                <div>
                  Something went wrong. Head back{' '}
                  <Link to="/" onClick={this.resetState}>
                    Home
                  </Link>
                  .
                </div>
                <br />
                <div>
                  You can report this issue on our{' '}
                  <a href="https://github.com/hasura/graphql-engine/issues">
                    GitHub
                  </a>{' '}
                  or chat with us on{' '}
                  <a href="http://discord.gg/hasura">Discord</a>
                </div>
              </div>
              <div className="col-xs-4">
                <img
                  src={errorImage}
                  className="img-responsive"
                  name="hasura"
                  title="Something went wrong!"
                />
              </div>
            </div>
          </div>
        </div>
      );
    }

    return this.props.children;
  }
}

ErrorBoundary.propTypes = {
  children: PropTypes.element,
};

export default ErrorBoundary;
