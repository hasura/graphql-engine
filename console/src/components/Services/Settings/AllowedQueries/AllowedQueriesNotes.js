import React from 'react';

import { Text } from '../../../UIKit/atoms';
import styles from './AllowedQueries.scss';

const AllowedQueriesNotes = () => (
  <div>
    <div>
      If GraphQL Engine is started with the{' '}
      <code>HASURA_GRAPHQL_ENABLE_ALLOWLIST</code> env var or the{' '}
      <code>--enable-allowlist</code> flag set to <i>true</i>, only queries
      added to the allow-list will be allowed to be executed.&nbsp;
      <a
        href="https://hasura.io/docs/1.0/graphql/manual/deployment/allow-list.html"
        target="_blank"
        rel="noopener noreferrer"
      >
        <i>(Read more)</i>
      </a>
    </div>
    <div className={styles.add_mar_top}>
      <Text fontWeight="bold">Notes</Text>
      <div className={styles.subsection}>
        <ul className={styles.ul_left_small + ' ' + styles.add_mar_top_small}>
          <li>All allowed queries need to have a unique name for reference</li>
        </ul>
      </div>
    </div>
  </div>
);

export default AllowedQueriesNotes;
