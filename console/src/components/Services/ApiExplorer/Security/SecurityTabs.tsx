import React from 'react';
import Helmet from 'react-helmet';
import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import { RightContainer } from '../../../Common/Layout/RightContainer';
import styles from '../../../Common/Common.scss';

const appPrefix = `/api`;

const tabs = {
  api_limits: {
    display_text: 'API Limits',
  },
  introspection: {
    display_text: 'Schema Introspection',
  },
};

export const SecurityTabs: React.FC<{ tabName: keyof typeof tabs }> = ({
  children,
  tabName,
}) => {
  const breadCrumbs = [
    {
      title: 'Security Settings',
      url: `${appPrefix}/security/api_limits`,
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
          heading={tabs[tabName].display_text}
          tabsInfo={tabs}
          breadCrumbs={breadCrumbs}
          baseUrl={`${appPrefix}/security`}
          showLoader={false}
          testPrefix="security-features-tabs"
        />
        <div className={styles.add_pad_top}>{children}</div>
      </div>
    </RightContainer>
  );
};
