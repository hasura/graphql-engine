import React from 'react';

import BreadCrumb from '../BreadCrumb/BreadCrumb';
import Tabs from '../ReusableTabs/ReusableTabs';

class CommonTabLayout extends React.Component {
  render() {
    const styles = require('./CommonTabLayout.scss');
    const {
      breadCrumbs,
      heading,
      appPrefix,
      currentTab,
      tabsInfo,
      baseUrl,
      showLoader,
      testPrefix,
    } = this.props;

    return (
      <div className={styles.subHeader}>
        <BreadCrumb breadCrumbs={breadCrumbs} />
        <h2 className={styles.heading_text + ' ' + styles.set_line_height}>
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
  }
}

export default CommonTabLayout;
