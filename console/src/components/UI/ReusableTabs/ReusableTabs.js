import React from 'react';
import { Tab, Tabs, TabList, TabPanel } from 'react-tabs';
import PropTypes from 'prop-types';
import styles from './ReusableTabs.scss';

class ReusableTabs extends React.Component {
  constructor() {
    super();

    this.state = {
      tabIndex: 0,
    };
  }
  render () {
    const { tabs } = this.props;
    const tabTitleList = [];
    const tabContentList = [];
    tabs.forEach((tab, i) => {
      tabTitleList.push(
        <Tab className={styles.commonTabList + (( this.state.tabIndex === i ) ? ' ' + styles.tabisActive : '')}>{ tab.title }</Tab>
      );
      tabContentList.push(<TabPanel>{tab.tabContent}</TabPanel>);
    })
    return (
      <Tabs className={styles.commonNavWrapper} selectedIndex={this.state.tabIndex} onSelect={tabIndex => this.setState({ tabIndex })}>
        <TabList className={styles.commonNavTab}>
          { tabTitleList }
        </TabList>
        { tabContentList }
      </Tabs>
    );
  }
};
ReusableTabs.PropTypes = {
  tabs: PropTypes.string.isRequired,
}
export default ReusableTabs;
