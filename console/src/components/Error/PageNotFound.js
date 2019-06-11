import PropTypes from 'prop-types';
import React, { Component } from 'react';
import { connect } from 'react-redux';

import { Link } from 'react-router';
import Helmet from 'react-helmet';

export class NotFoundError extends Error {}

class PageNotFound extends Component {
  render() {
    const errorImage = require('./error-logo.png');
    const styles = require('./ErrorPage.scss');

    const { resetCallback } = this.props;

    return (
      <div className={styles.viewContainer}>
        <Helmet title="404 - Page Not Found | Hasura" />
        <div className={'container ' + styles.centerContent}>
          <div className={'row ' + styles.message}>
            <div className="col-xs-8">
              <h1>404</h1>
              <br />
              <div>
                This page doesn't exist. Head back{' '}
                <Link to="/" onClick={resetCallback}>
                  Home
                </Link>
                .
              </div>
            </div>
            <div className="col-xs-4">
              <img
                src={errorImage}
                className="img-responsive"
                name="hasura"
                title="We think you are lost!"
              />
            </div>
          </div>
        </div>
      </div>
    );
  }
}

PageNotFound.propTypes = {
  dispatch: PropTypes.func.isRequired,
  resetCallback: PropTypes.func.isRequired,
};

export default connect()(PageNotFound);
