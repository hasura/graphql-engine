import React from 'react';
import PropTypes from 'prop-types';
import { connect } from 'react-redux';
import { Link as RouterLink } from 'react-router';
import Helmet from 'react-helmet';

import globals from '../../Globals';
import { Heading } from '../UIKit/atoms';
import styles from './ErrorPage.scss';

export class NotFoundError extends Error {}

const errorImage = `${globals.assetsPath}/common/img/hasura_icon_green.svg`;

const PageNotFound = ({ resetCallback }) => (
  <div className={styles.viewContainer}>
    <Helmet title="404 - Page Not Found | Hasura" />
    <div className={'container ' + styles.centerContent}>
      <div className={'row ' + styles.message}>
        <div className="col-xs-8">
          <Heading fontSize="54px" color="#333">
            404
          </Heading>
          <br />
          <div>
            This page doesn't exist. Head back{' '}
            <RouterLink to="/" onClick={resetCallback}>
              Home
            </RouterLink>
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

PageNotFound.propTypes = {
  dispatch: PropTypes.func.isRequired,
  resetCallback: PropTypes.func.isRequired,
};

export default connect()(PageNotFound);
