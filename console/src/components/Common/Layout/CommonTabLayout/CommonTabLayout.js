import React from 'react';

import BreadCrumb from '../BreadCrumb/BreadCrumb';
import Tabs from '../ReusableTabs/ReusableTabs';
import { Heading } from '../../../UIKit/atoms';
import styles from './CommonTabLayout.scss';

const CommonTabLayout = props => {
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
      <Heading
        as="h2"
        fontSize="18px"
        pb="20px"
        className={styles.set_line_height}
      >
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
};

export default CommonTabLayout;
