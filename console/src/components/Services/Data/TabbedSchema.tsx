import React from 'react';
import Helmet from 'react-helmet';

import CommonTabLayout from '../../Common/Layout/CommonTabLayout/CommonTabLayout';
import { RightContainer } from '../../Common/Layout/RightContainer';
import schemaStyles from './Schema/styles.scss';
import { Tabs } from '../../Common/Layout/ReusableTabs/ReusableTabs';

import styles from '../../../components/Common/Common.scss';
import { isSchemaSharingEnabled } from './Schema/SchemaSharing/schemaSharingConfig';
import { getDataSourceBaseRoute } from '../../Common/utils/routesUtils';
import BreadCrumb from '../../Common/Layout/BreadCrumb/BreadCrumb';

const tabs: Tabs = {
  display: {
    display_text: 'Schema',
  },
  gallery: {
    display_text: 'Schema Gallery',
  },
};

const appPrefix = '/data';

const TabbedSchema: React.FC<{
  tabName: 'display' | 'gallery';
  currentDataSource: string;
}> = ({ children, tabName, currentDataSource }) => {
  if (!isSchemaSharingEnabled) {
    return (
      <RightContainer>
        <Helmet title={`${tabs[tabName]?.display_text} - Hasura`} />
        <div
          className={`${styles.view_stitch_schema_wrapper} ${styles.addWrapper}`}
        >
          <BreadCrumb
            breadCrumbs={[
              { url: `/data`, title: 'Data' },
              {
                url: getDataSourceBaseRoute(currentDataSource),
                title: currentDataSource,
                prefix: <i className="fa fa-database" />,
              },
            ]}
          />
          {children}
        </div>
      </RightContainer>
    );
  }
  const breadCrumbs = [
    {
      title: 'Data',
      url: appPrefix,
    },
    {
      title: `${currentDataSource}`,
      url: '',
    },
  ];
  return (
    <RightContainer>
      <Helmet title={`${tabs[tabName]?.display_text} - Hasura`} />
      <div
        className={`${styles.view_stitch_schema_wrapper} ${styles.addWrapper}`}
      >
        <CommonTabLayout
          appPrefix={appPrefix}
          currentTab={tabName}
          heading={
            <div className={schemaStyles.tab_heading_flex}>
              <span
                className={`${schemaStyles.mr_xxs} ${schemaStyles.ion_icon}`}
              >
                <i className="fa fa-database" />
              </span>
              {currentDataSource}
            </div>
          }
          tabsInfo={tabs}
          breadCrumbs={breadCrumbs}
          baseUrl={`${appPrefix}/${currentDataSource}`}
          showLoader={false}
          testPrefix="schema-container-tabs"
        />
        <div className={styles.add_pad_top}>{children}</div>
      </div>
    </RightContainer>
  );
};

export default TabbedSchema;
