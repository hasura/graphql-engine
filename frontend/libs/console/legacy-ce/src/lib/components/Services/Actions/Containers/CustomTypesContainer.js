import React from 'react';
import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import { Analytics, REDACT_EVERYTHING } from '../../../../features/Analytics';
import tabInfo from './customTypesTabs';
import { appPrefix } from '../constants';
import styles from '../Actions.module.scss';

const CustomTypesContainer = ({ children, tabName }) => {
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
    <Analytics name="CustomTypesContainer" {...REDACT_EVERYTHING}>
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
    </Analytics>
  );
};

export default CustomTypesContainer;
