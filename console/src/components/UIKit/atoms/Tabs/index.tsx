import React, { useState } from 'react';

import {
  StyledTab,
  StyledTabList,
  StyledTabListItem,
  StyledTabContent,
} from './Tabs';
import { Props } from './typings'

export const Tabs = (props: Props) => {
  const [currentActiveTabIndex, changeCurrentActiveTab] = useState(0);
  const { tabsData } = props;

  const currentTabContent =
    tabsData && tabsData.filter((_, index) => index === currentActiveTabIndex);

  if (tabsData && tabsData.length > 0) {
    return (
      <StyledTab {...props}>
        <StyledTabList>
          {tabsData.map(({ title }, index) => (
            <StyledTabListItem
              key={title}
              selected={index === currentActiveTabIndex}
              onClick={() => changeCurrentActiveTab(index)}
            >
              {title}
            </StyledTabListItem>
          ))}
        </StyledTabList>
        {currentTabContent &&
          currentTabContent.map(({ tabContent }, index) => (
            <StyledTabContent key={index}>{tabContent}</StyledTabContent>
          ))}
      </StyledTab>
    );
  }

  // In case when we forget to pass tabs data.
  return <p>Please provide data for tabs</p>;
};
