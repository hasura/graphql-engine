import React from 'react';
import { Link } from 'react-router';

export type Tabs = {
  [tabName: string]: {
    display_text: string;
  };
};

type TabsProps = {
  appPrefix: string;
  tabsInfo: Tabs;
  tabName: string;
  count?: number;
  baseUrl: string;
  showLoader: boolean;
  testPrefix: string;
};

const Tabs = ({
  appPrefix,
  tabsInfo,
  tabName,
  count,
  baseUrl,
  showLoader,
  testPrefix,
}: TabsProps) => {
  let showCount = '';
  if (!(count === null || count === undefined)) {
    showCount = '(' + count + ')';
  }
  const styles = require('./ReusableTabs.scss');
  const dataLoader = () => {
    return (
      <span className={styles.loader_ml}>
        <i className="fa fa-spinner fa-spin" />
      </span>
    );
  };
  return (
    <React.Fragment>
      <div className={styles.common_nav} key={'reusable-tabs-1'}>
        <ul className="nav nav-pills">
          {Object.keys(tabsInfo).map((t: string, i) => (
            <li
              role="presentation"
              className={tabName === t ? styles.active : ''}
              key={i}
            >
              <Link
                to={`${baseUrl}/${t}`}
                data-test={`${
                  testPrefix ? testPrefix + '-' : ''
                }${appPrefix.slice(1)}-${t}`}
              >
                {tabsInfo[t].display_text} {tabName === t ? showCount : null}
                {tabName === t && showLoader ? dataLoader() : null}
              </Link>
            </li>
          ))}
        </ul>
      </div>
      <div className="clearfix" key={'reusable-tabs-2'} />
    </React.Fragment>
  );
};

export default Tabs;
