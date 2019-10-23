import React from 'react';

const Tabs = ({
  activeColor = '#ffffff',
  inactiveColor = '#F8FAFB',
  tabHeightPx = 50,
  activeTabIndex,
  tabs,
  borderColor = '#ccc',
  tabMarginBottomPx = 10,
  minHeightPx = 400,
  tabSwitchCallback,
  borderWidth = 1,
  padding = 20,
}) => {
  const [currentTab, setCurrentTab] = React.useState(activeTabIndex);

  const changeTab = index => {
    const oldTab = currentTab;
    setCurrentTab(index);
    if (tabSwitchCallback) {
      tabSwitchCallback(tabs[index], tabs[oldTab]);
    }
  };

  const getTab = (heading, index) => {
    const isCurrentActive = index === currentTab;
    let tabStyle = {
      display: 'flex',
      flexDirection: 'column',
      background: isCurrentActive ? activeColor : inactiveColor,
      width: '100%',
      height: '100%',
      justifyContent: 'center',
      alignItems: 'center',
      cursor: 'pointer',
      borderWidth: borderWidth,
      borderColor,
      borderBottomStyle: 'solid',
    };

    if (isCurrentActive) {
      // tabStyle = {
      //   ...tabStyle,
      //   borderRightWidth: borderWidth,
      //   borderRightColor: borderColor,
      //   borderRightStyle: 'solid',
      // };
      tabStyle = {
        ...tabStyle,
        borderRightStyle: 'solid',
        borderLeftStyle: 'solid',
        borderBottomStyle: 'none',
      };
    }

    if (index === 0) {
      tabStyle = {
        ...tabStyle,
        borderLeftStyle: 'solid',
      };
    }

    return (
      <div onClick={() => changeTab(index)} style={tabStyle}>
        {isCurrentActive ? <b>{heading}</b> : heading}
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
        minHeight: minHeightPx,
        border: `${borderWidth}px solid ${borderColor}`,
        borderRadius: 5,
      }}
    >
      {getTabHeader()}
      <div style={{ padding }}>{tabs[currentTab].component}</div>
    </div>
  );
};

export default Tabs;
