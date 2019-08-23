import PropTypes from 'prop-types';
import React, { Component } from 'react';
import { connect } from 'react-redux';

import Helmet from 'react-helmet';

class PageNotFound extends Component {
  render() {
    const styles = require('./Styles.scss');
    return (
      <div className={styles.viewcontainer}>
        <Helmet title="404 - Page Not Found" />
        <div className={'container ' + styles.centerContent}>
          <div className={'row ' + styles.message}>
            <div className="col-xs-8">
              <h1>404</h1>
              This page doesn't exist.
            </div>
          </div>
        </div>
      </div>
    );
  }
}

PageNotFound.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

export default connect()(PageNotFound);
