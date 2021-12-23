import React from 'react';
import { connect } from 'react-redux';

import { Link } from 'react-router';
import Helmet from 'react-helmet';
import globals from '../../Globals';
import styles from './ErrorPage.scss';

export class NotFoundError extends Error {}

type PageNotFoundProps = {
  resetCallback: () => void;
};

const PageNotFound = (props: PageNotFoundProps) => {
  const errorImage = `${globals.assetsPath}/common/img/hasura_icon_green.svg`;

  const { resetCallback } = props;

  return (
    <div className={styles.viewContainer}>
      <Helmet title="404 - Page Not Found | Hasura" />
      <div className={`container  ${styles.centerContent}`}>
        <div className={`row ${styles.message}`}>
          <div className="col-xs-8">
            <h1>404</h1>
            <br />
            <div>
              This page does not exist. Head back{' '}
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
              title="We think you are lost!"
              alt="Not found"
            />
          </div>
        </div>
      </div>
    </div>
  );
};

export default connect()(PageNotFound);
