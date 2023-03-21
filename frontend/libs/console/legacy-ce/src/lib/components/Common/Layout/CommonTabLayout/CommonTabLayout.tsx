import React from 'react';
import clsx from 'clsx';

import BreadCrumb, {
  BreadCrumb as BreadCrumbType,
} from '../BreadCrumb/BreadCrumb';
import Tabs, { Tabs as TabsType } from '../ReusableTabs/ReusableTabs';
import styles from './CommonTabLayout.module.scss';

type Props = {
  breadCrumbs: BreadCrumbType[];
  heading: React.ReactNode;
  appPrefix: string;
  currentTab: string;
  tabsInfo: TabsType;
  baseUrl: string;
  showLoader: boolean;
  testPrefix: string;
  subHeading?: React.ReactNode;
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
    subHeading,
  } = props;

  return (
    <div className={clsx(styles.subHeader, 'bootstrap-jail')}>
      {breadCrumbs.length ? <BreadCrumb breadCrumbs={breadCrumbs} /> : null}
      <h2 className={`${styles.heading_text} ${styles.set_line_height}`}>
        {heading || ''}
      </h2>
      {subHeading || null}
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
