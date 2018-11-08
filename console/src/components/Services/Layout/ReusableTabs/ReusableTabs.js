import React from 'react';
import { Link } from 'react-router';

const Tabs = ({
  appPrefix,
  tabsInfo,
  tabName,
  count,
  baseUrl,
}) => {
  let showCount = '';
  if (!(count === null || count === undefined)) {
    showCount = '(' + count + ')';
  }
  const styles = require('./ReusableTabs.scss');
  return [
    <div className={styles.common_nav}>
      <ul className="nav nav-pills">
        {Object.keys(tabsInfo).map((t, i) => (
          <li
            role="presentation"
            className={tabName === t ? styles.active : ''}
            key={i}
          >
            <Link
              to={`${baseUrl}/${t}`}
              data-test={`${appPrefix.slice(1)}-${t}`}
            >
              {tabsInfo[t].display_text} {tabName === t ? showCount : null}
            </Link>
          </li>
        ))}
      </ul>
    </div>,
    <div className="clearfix" />,
  ];
};

export default Tabs;
