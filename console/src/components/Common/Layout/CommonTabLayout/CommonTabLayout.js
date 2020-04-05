import React from 'react';

import BreadCrumb from '../BreadCrumb/BreadCrumb';
import Tabs from '../ReusableTabs/ReusableTabs';
import { Heading } from '../../../UIKit/atoms';
import styles from './CommonTabLayout.scss';

const CommonTabLayout = ({
  breadCrumbs,
  heading,
  appPrefix,
  currentTab,
  tabsInfo,
  baseUrl,
  showLoader,
  testPrefix,
}) => (
  <div className={styles.subHeader}>
    <BreadCrumb breadCrumbs={breadCrumbs} />
    <Heading as="h2" fontSize="18px" pb="20px" lineHeight="26px">
      {heading || ''}
    </Heading>
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

export default CommonTabLayout;
