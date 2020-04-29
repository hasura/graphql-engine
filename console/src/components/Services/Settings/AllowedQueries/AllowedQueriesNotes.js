import React from 'react';

import { Text, Link } from '../../../UIKit/atoms';
import styles from './AllowedQueries.scss';

const AllowedQueriesNotes = () => (
  <div>
    <div>
      If GraphQL Engine is started with the{' '}
      <code>HASURA_GRAPHQL_ENABLE_ALLOWLIST</code> env var or the{' '}
      <code>--enable-allowlist</code> flag set to <i>true</i>, only queries
      added to the allow-list will be allowed to be executed.&nbsp;
      <Link
        href="https://hasura.io/docs/1.0/graphql/manual/deployment/allow-list.html"
        type="moreInfo"
        fontSize="14px"
      >
        Read more
      </Link>
    </div>
    <Text fontWeight="bold" mt="20px">
      Notes
    </Text>
    <div className={styles.subsection}>
      <ul className={styles.ul_left_small + ' ' + styles.add_mar_top_small}>
        <li>All allowed queries need to have a unique name for reference</li>
      </ul>
    </div>
  </div>
);

export default AllowedQueriesNotes;
