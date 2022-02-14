import React from 'react';
import Helmet from 'react-helmet';
import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import { RightContainer } from '../../../Common/Layout/RightContainer';
import tabs from './tabs';
import styles from '../../../Common/Common.scss';

const appPrefix = '/data';

const TabbedDSConnection: React.FC<{
  tabName: 'create' | 'connect';
}> = ({ children, tabName }) => {
  const breadCrumbs = [
    {
      title: 'Data',
      url: appPrefix,
    },
    {
      title: 'Data Manager',
      url: `${appPrefix}/manage`,
    },
    {
      title: tabs[tabName].display_text,
      url: '',
    },
  ];

  return (
    <RightContainer>
      <Helmet title={`${tabs[tabName].display_text} - Hasura`} />
      <div
        className={`${styles.view_stitch_schema_wrapper} ${styles.addWrapper}`}
      >
        <CommonTabLayout
          appPrefix={appPrefix}
          currentTab={tabName}
          heading="Connect Database"
          tabsInfo={tabs}
          breadCrumbs={breadCrumbs}
          baseUrl={`${appPrefix}/manage`}
          showLoader={false}
          testPrefix="datasource-container-tabs"
        />
        <div className={styles.add_pad_top}>{children}</div>
      </div>
    </RightContainer>
  );
};

export default TabbedDSConnection;
