import React from 'react';
import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import tabInfo from './customTypesTabs';
import { appPrefix } from '../constants';

const CustomTypesContainer = ({ children, tabName }) => {
  const styles = require('../Actions.scss');

  const breadCrumbs = [
    {
      title: 'Actions',
      url: appPrefix,
    },
    {
      title: 'Types',
      url: `${appPrefix}/types`,
    },
    {
      title: tabName,
      url: '',
    },
  ];

  return (
    <div
      className={styles.view_stitch_schema_wrapper + ' ' + styles.addWrapper}
    >
      <CommonTabLayout
        appPrefix={appPrefix}
        currentTab={tabName}
        heading="Custom Types"
        tabsInfo={tabInfo}
        breadCrumbs={breadCrumbs}
        baseUrl={`${appPrefix}/types`}
      />
      <div className={styles.add_pad_top}>{children}</div>
    </div>
  );
};

export default CustomTypesContainer;
