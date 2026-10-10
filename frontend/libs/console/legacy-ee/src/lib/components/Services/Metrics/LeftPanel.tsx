import { Link, useLocation } from 'react-router';
import { relativeModulePath } from './constants';
import { strippedCurrUrl } from './helpers';
import overview from './images/overviewNew.svg';
import errors from './images/errorsNew.svg';
import operation from './images/operation.svg';
import usage from './images/usageNew.svg';
import styles from './Metrics.module.scss';

const LeftPanel = () => {
  const location = useLocation();
  const strippedUrl = strippedCurrUrl(location.pathname);

  return (
    <div className={styles['LeftPanelWrapper']}>
      <ul className={styles['ul_pad_remove']}>
        <Link to={relativeModulePath}>
          <li
            className={
              strippedUrl === relativeModulePath ? styles['active'] : ''
            }
          >
            <img src={overview} alt={'Overview'} /> Overview
          </li>
        </Link>
        <Link to={`${relativeModulePath}/error`}>
          <li
            className={
              strippedUrl === `${relativeModulePath}/error`
                ? styles['active']
                : ''
            }
          >
            <img src={errors} alt={'Errors'} /> Errors
          </li>
        </Link>
        <Link to={`${relativeModulePath}/usage`}>
          <li
            className={
              strippedUrl === `${relativeModulePath}/usage`
                ? styles['active']
                : ''
            }
          >
            <img src={usage} alt={'usage'} /> Usage
          </li>
        </Link>
        <Link to={`${relativeModulePath}/operations`}>
          <li
            className={
              strippedUrl === `${relativeModulePath}/operations`
                ? styles['active']
                : ''
            }
          >
            <img src={operation} alt={'operation'} /> Operations
          </li>
        </Link>
      </ul>
    </div>
  );
};

export default LeftPanel;
