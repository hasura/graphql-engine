import React from 'react';
import PropTypes from 'prop-types';
import { connect } from 'react-redux';
import { Link as RouterLink } from 'react-router';
import Helmet from 'react-helmet';

import globals from '../../Globals';
import { Heading, Link } from '../UIKit/atoms';
import styles from './ErrorPage.scss';

const errorImage = `${globals.assetsPath}/common/img/hasura_icon_green.svg`;

const RuntimeError = ({ resetCallback, error }) => (
  <div className={styles.viewContainer}>
    <Helmet title="Error | Hasura" />
    <div className={'container ' + styles.centerContent}>
      <div className={'row ' + styles.message}>
        <div className="col-xs-8">
          <Heading fontSize="54px" color="#333">
            Error
          </Heading>
          <br />
          <div>
            Something went wrong. Head back{' '}
            <RouterLink to="/" onClick={resetCallback}>
              Home
            </RouterLink>
            .
          </div>
          <br />
          <div>
            <pre className={styles.errorStack}>{error.stack}</pre>
          </div>
          <br />
          <div>
            You can report this issue on our
            <Link
              mx="xs"
              href="https://github.com/hasura/graphql-engine/issues"
              hover="underline"
            >
              GitHub
            </Link>
            or chat with us on
            <Link href="http://discord.gg/hasura" hover="underline" ml="xs">
              Discord
            </Link>
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

RuntimeError.propTypes = {
  dispatch: PropTypes.func.isRequired,
  resetCallback: PropTypes.func.isRequired,
};

export default connect()(RuntimeError);
