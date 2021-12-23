import React from 'react';
import { Link } from 'react-router';
import styles from './ReusableTabs.scss';

export type Tabs = Record<
  string,
  { display_text: string; display?: React.ReactNode }
>;

type Props = {
  appPrefix: string;
  tabsInfo: Tabs;
  tabName: string;
  count?: number;
  baseUrl: string;
  showLoader: boolean;
  testPrefix: string;
};

const Tabs: React.FC<Props> = ({
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
    showCount = `(${count})`;
  }
  return (
    <React.Fragment>
      <div className={styles.common_nav} key="reusable-tabs-1">
        <ul className="nav nav-pills">
          {Object.keys(tabsInfo).map((t: string) => (
            <li
              role="presentation"
              className={tabName === t ? styles.active : ''}
              key={t}
            >
              <Link
                to={`${baseUrl}/${t}`}
                data-test={`${
                  testPrefix ? `${testPrefix}-` : ''
                }${appPrefix.slice(1)}-${t}`}
              >
                {tabsInfo[t].display || tabsInfo[t].display_text}{' '}
                {tabName === t ? showCount : null}
                {tabName === t && showLoader ? (
                  <span className={styles.loader_ml}>
                    <i className="fa fa-spinner fa-spin" />
                  </span>
                ) : null}
              </Link>
            </li>
          ))}
        </ul>
      </div>
      <div className="clearfix" key="reusable-tabs-2" />
    </React.Fragment>
  );
};

export default Tabs;
