import React from 'react';
import styles from './AllowedQueries.scss';

class AllowedQueriesNotes extends React.Component {
  render() {
    return (
      <div className={styles.wd68}>
        <b>Notes</b>
        <ul className={styles.ul_left_small + ' ' + styles.add_mar_top_small}>
          <li>
            If GraphQL Engine is started with the{' '}
            <code>HASURA_GRAPHQL_ENABLE_ALLOWLIST</code> env var or the{' '}
            <code>--enable-allowlist</code> flag set to <i>true</i>, only
            queries added to the allow-list will be allowed to be executed
          </li>
          <li>This is recommended for production GraphQL Engine instances</li>
        </ul>
      </div>
    );
  }
}

export default AllowedQueriesNotes;
