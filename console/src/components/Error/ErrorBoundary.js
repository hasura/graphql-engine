import React from 'react';
import PropTypes from 'prop-types';

import { Link } from 'react-router';
import Helmet from 'react-helmet';

class ErrorBoundary extends React.Component {
  constructor(props) {
    super(props);
    this.state = { hasError: false, info: null, error: null };
  }

  componentDidCatch(error, info) {
    this.setState({ hasError: true, info: info, error: error });
    // TODO logErrorToMyService(error, info);
  }

  render() {
    const errorImage = require('./error-logo.png');
    const styles = require('./ErrorPage.scss');

    if (this.state.hasError) {
      return (
        <div className={styles.viewContainer}>
          <Helmet title="Error | Hasura" />
          <div className={'container ' + styles.centerContent}>
            <div className={'row ' + styles.message}>
              <div className="col-xs-8">
                <h1>Error</h1>
                <br />
                <div>
                  Something went wrong. Head back <Link to="/">Home</Link>.
                </div>
                <br />
                <div>
                  You can report this issue on our{' '}
                  <a href="https://github.com/hasura/graphql-engine/issues">
                    Github
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
