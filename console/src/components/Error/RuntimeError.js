import PropTypes from 'prop-types';
import React, { Component } from 'react';
import { connect } from 'react-redux';

import { Link } from 'react-router';
import Helmet from 'react-helmet';
import globals from '../../Globals';

class RuntimeError extends Component {
  render() {
    const errorImage = `${globals.assetsPath}/common/img/hasura_icon_green.svg`;
    const styles = require('./ErrorPage.scss');

    const { resetCallback, error } = this.props;

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
                <Link to="/" onClick={resetCallback}>
                  Home
                </Link>
                .
              </div>
              <br />
              <div>
                <pre className={styles.errorStack}>{error.stack}</pre>
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
}

RuntimeError.propTypes = {
  dispatch: PropTypes.func.isRequired,
  resetCallback: PropTypes.func.isRequired,
};

export default connect()(RuntimeError);
