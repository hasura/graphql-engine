import React from 'react';
import Helmet from 'react-helmet';
import { Link } from 'react-router';

const appPrefix = '/custom-resolver';

class CustomResolver extends React.Component {
  render() {
    const styles = require('../Styles.scss');
    return (
      <div className={styles.resolverWrapper}>
        <Helmet title={'Custom Resolvers | Hasura'} />
        <div className={styles.resolverContent}>
          Add pre-CURD custom business logic like data validation, etc. or
          stitch an existing or custom GraphQL schema
        </div>
        <div className={styles.resolverImg} />
        <div className={styles.commonBtn}>
          <Link
            className={styles.padd_remove_full}
            to={`${appPrefix}/manage/add`}
          >
            <button className={styles.yellow_button}>
              Add Remote GraphQL schema
            </button>
          </Link>
        </div>
        <div className={styles.readMore}>
          <a href="#" target="_blank">
            Read more
          </a>
        </div>
      </div>
    );
  }
}

const landingCustomResolverGen = connect => connect()(CustomResolver);

export default landingCustomResolverGen;
