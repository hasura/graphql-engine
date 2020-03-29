import React from 'react';

import ClearAdminSecret from './ClearAdminSecret';
import { Heading } from '../../../UIKit/atoms';
import styles from '../Settings.scss';

const Logout = props => (
  <div
    className={`${styles.clear_fix} ${styles.padd_left} ${styles.padd_top} ${styles.metadata_wrapper} container-fluid`}
  >
    <div className={styles.subHeader}>
      <Heading as="h2" pb="0px" fontSize="18px">
        Logout (clear admin-secret)
      </Heading>
    </div>

    <div>
      <div key="access_key_reset_1" className={styles.intro_note}>
        <div className={styles.content_width}>
          The console caches the admin-secret (HASURA_GRAPHQL_ADMIN_SECRET) in
          the browser. You can clear this cache to force a prompt for the
          admin-secret when the console is accessed next using this browser.
        </div>
      </div>

      <div key="access_key_reset_2">
        <ClearAdminSecret {...props} />
      </div>
    </div>
  </div>
);

const mapStateToProps = state => {
  return {
    ...state.main,
    metadata: state.metadata,
    dataHeaders: { ...state.tables.dataHeaders },
  };
};

const logoutConnector = connect => connect(mapStateToProps)(Logout);

export default logoutConnector;
