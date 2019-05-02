import React from 'react';
import { Tab, Tabs, TabList, TabPanel } from 'react-tabs';
import styles from './Navigation.scss';

class Navigation extends React.Component {
  constructor() {
    super();

    this.state = {
      tabIndex: 0,
    };
  }
  render () {
    return (
      <Tabs className={styles.commonNavWrapper} selectedIndex={this.state.tabIndex} onSelect={tabIndex => this.setState({ tabIndex })}>
        <TabList className={styles.commonNavTab}>
          <Tab className={styles.commonTabList + (( this.state.tabIndex === 0 ) ? ' ' + styles.tabisActive : '')}>Title 1</Tab>
          <Tab className={styles.commonTabList + (( this.state.tabIndex === 1 ) ? ' ' + styles.tabisActive : '' )}>Title 2</Tab>
          <Tab className={styles.commonTabList + (( this.state.tabIndex === 2 ) ? ' ' + styles.tabisActive : '' )}>Title 3</Tab>
        </TabList>
        <TabPanel>
          1111111
        </TabPanel>
        <TabPanel>
          222222
        </TabPanel>
        <TabPanel>
          333333
        </TabPanel>
      </Tabs>
    );
  }
};

export default Navigation;
