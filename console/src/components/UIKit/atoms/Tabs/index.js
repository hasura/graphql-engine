import React from 'react';
import PropTypes from 'prop-types';

import { TabStyles, TabList, TabListItem, TabContent } from './Tabs.style';

class Tabs extends React.Component {
  state = {
    // Current Active Tab
    currentActiveTabIndex: 0,
  };

  // function to change current active tab

  changeCurrentActiveTab = tabIndex =>
    this.setState({ currentActiveTabIndex: tabIndex });

  // ************************** //

  render() {
    const { currentActiveTabIndex } = this.state;

    const { tabsData } = this.props;

    // Current Active Tab Content

    const currentTabContentArray =
      tabsData &&
      tabsData.filter((currentElement, index) => {
        return index === currentActiveTabIndex;
      });

    // ****************** //

    // Tabs only going to render if the received tabsdata is truthy and atleast have some data inside.

    if (tabsData && tabsData.length > 0) {
      return (
        <TabStyles {...this.props}>
          {/* Tab Navigation */}
          <TabList>
            {tabsData &&
              tabsData.map(({ title }, index) => (
                <TabListItem
                  key={title}
                  // CSS-in-Js prop ~ Active Tab
                  selected={index === currentActiveTabIndex && true}
                  onClick={() => this.changeCurrentActiveTab(index)}
                >
                  {title}
                </TabListItem>
              ))}
          </TabList>
          {/* Tab Content */}
          {currentTabContentArray &&
            currentTabContentArray.map(({ tabContent }) => (
              <TabContent>{tabContent}</TabContent>
            ))}
        </TabStyles>
      );
    }
    // No else clause here.

    // In case when we forget to pass tabs data.

    return <p>Please provide data for tabs</p>;
  }
}

// PropTypes for Tabs ************* //

Tabs.propTypes = {
  tabsData: PropTypes.array.isRequired,
  border: PropTypes.number,
  borderColor: PropTypes.string,
};

// ******************************* //

export default Tabs;
