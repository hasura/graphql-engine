import React from 'react';

import BreadCrumb, {
  BreadCrumb as BreadCrumbType,
} from '../BreadCrumb/BreadCrumb';
import Tabs, { Tabs as TabsType } from '../ReusableTabs/ReusableTabs';
import styles from './CommonTabLayout.scss';

type Props = {
  breadCrumbs: BreadCrumbType[];
  heading: React.ReactNode;
  appPrefix: string;
  currentTab: string;
  tabsInfo: TabsType;
  baseUrl: string;
  showLoader: boolean;
  testPrefix: string;
};

const CommonTabLayout: React.FC<Props> = props => {
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
      {breadCrumbs.length ? <BreadCrumb breadCrumbs={breadCrumbs} /> : null}
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
