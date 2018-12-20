import React from 'react';

import BreadCrumb from '../../Layout/BreadCrumb/BreadCrumb';
import Tabs from '../../Layout/ReusableTabs/ReusableTabs';

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
        />
      </div>
    );
  }
}

export default CommonTabLayout;
