import React from 'react';

import { Link } from 'react-router';

import { relativeModulePath } from './constants';
/*
const performance = require('./images/performance.svg');
const rightArrow = require('./images/right-arrow.svg');
const downArrow = require('./images/down-arrow.svg');
*/
import { strippedCurrUrl } from './helpers';
import stop from './images/stop.svg';

import overview from './images/overviewNew.svg';
import errors from './images/errorsNew.svg';
import operation from './images/operation.svg';
import allowList from './images/allowlist.svg';
import usage from './images/usageNew.svg';
import refreshArrows from './images/refresh-arrows.svg';
import transfer from './images/transfer.svg';
import regressionTest from './images/check-circle.svg';
import styles from './Metrics.module.scss';

const LeftPanel = props => {
  const { location } = props;
  const { pathname } = location;
  const strippedUrl = strippedCurrUrl(pathname);

  /*
  const [toggleExpand, toggle] = useState(false);
  const toggleGenerated = () => {
    toggle(!toggleExpand);
  };
  */
  return (
    <div className={styles.LeftPanelWrapper}>
      <ul className={styles.ul_pad_remove}>
        <Link to={relativeModulePath}>
          {/*
          <li className={styles.active}>
          */}
          <li
            className={strippedUrl === relativeModulePath ? styles.active : ''}
          >
            <img src={overview} alt={'Overview'} /> Overview
            {/*
            <div className={styles.deadBtn}>
              <button>DEAD</button>
            </div>
            */}
          </li>
        </Link>
        {/*
        <li onClick={toggleGenerated}>
          <img src={performance} alt={'Performance'} /> Performance
          <span className={styles.arrowPos}>
            <img
              src={toggleExpand ? downArrow : rightArrow}
              alt={toggleExpand ? 'Right arrow' : 'Down arrow'}
            />
          </span>
        </li>
        {toggleExpand ? (
          <div className={styles.projectList}>
            <ul>
              <li>
                <div className={styles.listCircle} />
                Slow queries
              </li>
              <li>
                <div className={styles.listCircle} />
                Query variations
              </li>
            </ul>
          </div>
        ) : null}
        */}

        <Link to={`${relativeModulePath}/error`}>
          <li
            className={
              strippedUrl === `${relativeModulePath}/error` ? styles.active : ''
            }
          >
            <img src={errors} alt={'Errors'} /> Errors
          </li>
        </Link>
        <Link to={`${relativeModulePath}/usage`}>
          <li
            className={
              strippedUrl === `${relativeModulePath}/usage` ? styles.active : ''
            }
          >
            <img src={usage} alt={'usage'} /> Usage
          </li>
        </Link>
        <Link to={`${relativeModulePath}/operations`}>
          <li
            className={
              strippedUrl === `${relativeModulePath}/operations`
                ? styles.active
                : ''
            }
          >
            <img src={operation} alt={'operation'} /> Operations
          </li>
        </Link>
        <Link to={`${relativeModulePath}/websockets`}>
          <li
            className={
              strippedUrl === `${relativeModulePath}/websockets`
                ? styles.active
                : ''
            }
          >
            <img src={transfer} alt="Websockets" /> Websockets
          </li>
        </Link>
        <Link to={`${relativeModulePath}/subscription-workers`}>
          <li
            className={
              strippedUrl === `${relativeModulePath}/subscription-workers`
                ? styles.active
                : ''
            }
          >
            <img src={refreshArrows} alt="Subscription Workers" /> Subscription
            Workers
          </li>
        </Link>
        <Link to={`${relativeModulePath}/allow-lists`}>
          <li
            className={
              strippedUrl === `${relativeModulePath}/allow-lists`
                ? styles.active
                : ''
            }
          >
            <img src={allowList} alt={'Allow lists'} /> Allow Lists
          </li>
        </Link>
        <Link to={`${relativeModulePath}/api-limits`}>
          <li
            className={
              strippedUrl === `${relativeModulePath}/api-limits`
                ? styles.active
                : ''
            }
          >
            <img src={stop} alt="API Limits" /> API Limits
          </li>
        </Link>
        <Link to={`${relativeModulePath}/regression-tests`}>
          <li
            className={
              strippedUrl === `${relativeModulePath}/regression-tests`
                ? styles.active
                : ''
            }
          >
            <img src={regressionTest} alt="Regression Tests" /> Regression Tests
          </li>
        </Link>
      </ul>
    </div>
  );
};

export default LeftPanel;
