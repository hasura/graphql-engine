import React, { useState } from 'react';

import {
  StyledTab,
  StyledTabList,
  StyledTabListItem,
  StyledTabContent,
} from './Tabs';

export const Tabs = props => {
  const [currentActiveTabIndex, changeCurrentActiveTab] = useState(0);
  const { tabsData } = props;

  const currentTabContentArray =
    tabsData &&
    tabsData.filter((currentElement, index) => {
      return index === currentActiveTabIndex;
    });

  if (tabsData && tabsData.length > 0) {
    return (
      <StyledTab {...props}>
        {/* Tab Navigation */}
        <StyledTabList>
          {tabsData &&
            tabsData.map(({ title }, index) => (
              <StyledTabListItem
                key={title}
                selected={index === currentActiveTabIndex && true}
                onClick={() => changeCurrentActiveTab(index)}
              >
                {title}
              </StyledTabListItem>
            ))}
        </StyledTabList>
        {/* Tab Content */}
        {currentTabContentArray &&
          currentTabContentArray.map(({ tabContent }, index) => (
            <StyledTabContent key={index}>{tabContent}</StyledTabContent>
          ))}
      </StyledTab>
    );
  }

  // In case when we forget to pass tabs data.
  return <p>Please provide data for tabs</p>;
};
