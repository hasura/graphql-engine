import React from 'react';

import BreadCrumb, { BreadCrumbItem } from '../BreadCrumb/BreadCrumb';
import Tabs, { Tabs as TabsType } from '../ReusableTabs/ReusableTabs';
import styles from './CommonTabLayout.scss';

type TabLayoutProps = {
  breadCrumbs: BreadCrumbItem[];
  heading: string | JSX.Element;
  appPrefix: string;
  currentTab: string;
  tabsInfo: TabsType;
  baseUrl: string;
  showLoader: boolean;
  testPrefix: string;
};

const CommonTabLayout = (props: TabLayoutProps) => {
  const {
    breadCrumbs,
    heading,
    appPrefix,
    currentTab,
    tabsInfo,
    baseUrl,
    showLoader,
    testPrefix,
  } = props;

  return (
    <div className={styles.subHeader}>
      <BreadCrumb breadCrumbs={breadCrumbs} />
      <h2 className={`${styles.heading_text} ${styles.set_line_height}`}>
        {heading || ''}
      </h2>
      <Tabs
        appPrefix={appPrefix}
        tabName={currentTab}
        tabsInfo={tabsInfo}
        baseUrl={baseUrl}
        showLoader={showLoader}
        testPrefix={testPrefix}
      />
    </div>
  );
};

export default CommonTabLayout;
