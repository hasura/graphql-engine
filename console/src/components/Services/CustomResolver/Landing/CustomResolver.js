import React from 'react';
import Helmet from 'react-helmet';

import { fetchResolvers } from '../customActions';

class CustomResolver extends React.Component {
  componentDidMount() {
    this.props.dispatch(fetchResolvers());
  }
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
          <button className={styles.yellow_button}>
            Add Remote GraphQL schema
          </button>
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
