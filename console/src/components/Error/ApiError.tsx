import React from 'react';
import { connect } from 'react-redux';

import Helmet from 'react-helmet';
import globals from '../../Globals';
import styles from './ErrorPage.scss';

type ApiErrorProps = {
  error: any;
  requestURL: string;
};

const ApiError = (props: ApiErrorProps) => {

  const errorImage = `${globals.assetsPath}/common/img/hasura_icon_green.svg`;

  const { error, requestURL } = props;

  return (
    <div className={styles.viewContainer}>
      <Helmet title="Error | Hasura" />
      <div className={`container ${  styles.centerContent}`}>
        <div className={`row ${  styles.message}`}>
          <div className="col-xs-8">
            <h1>Error</h1>
            <br />
            <div>
              Something went wrong while trying to request <span className={styles.errorStack}>{requestURL}</span> .
            </div>
            <br />
            <div>
              <pre className={styles.errorStack}>
                {error.error}
              </pre>
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
              title="hasura"
              alt="Not found"
            />
          </div>
        </div>
      </div>
    </div>
  );
};

export default connect()(ApiError);
