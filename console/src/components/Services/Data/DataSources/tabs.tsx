import React from 'react';
import { Tabs } from '../../../Common/Layout/ReusableTabs/ReusableTabs';
import Globals from '../../../../Globals';
import styles from './DataSources.scss';

const tabs: Tabs = {
  connect: {
    display_text: 'Connect Existing Database',
  },
};
if (Globals.hasuraCloudTenantId && Globals.herokuOAuthClientId) {
  tabs.create = {
    display: (
      <div className={styles.display_flex}>
        <div className={styles.add_mar_right_mid}>Create Heroku Database</div>
        <div className={styles.free_badge}>Free</div>
      </div>
    ),
    display_text: 'Create Heroku Database',
  };
}

export default tabs;
