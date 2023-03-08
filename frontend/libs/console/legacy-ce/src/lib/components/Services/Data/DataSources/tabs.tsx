import React from 'react';
import Globals from '../../../../Globals';
import { Tabs } from '../../../Common/Layout/ReusableTabs/ReusableTabs';
import styles from './DataSources.module.scss';

const tabs: Tabs = {
  connect: {
    display_text: 'Connect Existing Database',
  },
};

// this condition is true only for Hasura Cloud projects
if (Globals.consoleType === 'cloud' && Globals.hasuraCloudTenantId) {
  const tabTitle = 'Create New Database';

  tabs.create = {
    display: (
      <div className={styles.display_flex}>
        <div className={styles.add_mar_right_mid}>{tabTitle}</div>
        <div className={styles.free_badge}>Free</div>
      </div>
    ),
    display_text: tabTitle,
  };
}

export default tabs;
