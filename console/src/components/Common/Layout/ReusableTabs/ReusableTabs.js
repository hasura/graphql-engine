import React from 'react';
import { Link as RouterLink } from 'react-router';

import { Icon } from '../../../UIKit/atoms';

const Tabs = ({
  appPrefix,
  tabsInfo,
  tabName,
  count,
  baseUrl,
  showLoader,
  testPrefix,
}) => {
  let showCount = '';
  if (!(count === null || count === undefined)) {
    showCount = '(' + count + ')';
  }

  const styles = require('./ReusableTabs.scss');
  const dataLoader = () => {
    return (
      <span className={styles.loader_ml}>
        <Icon type="spinner" />
      </span>
    );
  };

  return [
    <div className={styles.common_nav} key={'reusable-tabs-1'}>
      <ul className="nav nav-pills">
        {Object.keys(tabsInfo).map((t, i) => (
          <li
            role="presentation"
            className={tabName === t ? styles.active : ''}
            key={i}
          >
            <RouterLink
              to={`${baseUrl}/${t}`}
              data-test={`${
                testPrefix ? testPrefix + '-' : ''
              }${appPrefix.slice(1)}-${t}`}
            >
              {tabsInfo[t].display_text} {tabName === t ? showCount : null}
              {tabName === t && showLoader ? dataLoader() : null}
            </RouterLink>
          </li>
        ))}
      </ul>
    </div>,
    <div className="clearfix" key={'reusable-tabs-2'} />,
  ];
};

export default Tabs;
