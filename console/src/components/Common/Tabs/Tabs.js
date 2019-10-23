import React from 'react';

const Tabs = ({
  activeColor = '#ffffff',
  inactiveColor = '#ccc',
  tabHeightPx = 50,
  activeTabIndex,
  tabs,
  tabMarginBottomPx = 10,
}) => {
  const [currentTab, setCurrentTab] = React.useState(activeTabIndex);

  const getTab = (heading, index) => {
    return (
      <div
        onClick={() => setCurrentTab(index)}
        style={{
          display: 'flex',
          flexDirection: 'column',
          background: index === currentTab ? activeColor : inactiveColor,
          width: '100%',
          height: '100%',
          justifyContent: 'center',
          alignItems: 'center',
        }}
      >
        {heading}
      </div>
    );
  };

  const getTabHeader = () => {
    return (
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'space-around',
          height: tabHeightPx,
          marginBottom: tabMarginBottomPx,
        }}
      >
        {tabs.map((t, i) => getTab(t.heading, i))}
      </div>
    );
  };

  return (
    <div
      style={{
        display: 'flex',
        flexDirection: 'column',
        background: activeColor,
      }}
    >
      {getTabHeader()}
      {tabs[currentTab].component}
    </div>
  );
};

export default Tabs;
