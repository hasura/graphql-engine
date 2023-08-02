import moment from 'moment';
import React, { useState, useEffect } from 'react';
import { FaChevronDown } from 'react-icons/fa';
import { FiRefreshCw } from 'react-icons/fi';
import { Button } from '@hasura/console-legacy-ce';

import styles from '../MetricsV1.module.scss';

const BucketTypes = [
  { name: 'Last 10 Minutes', subtractFactor: [10, 'minutes'] },
  { name: 'Last Hour ', subtractFactor: [1, 'hour'] },
  { name: 'Last 8 Hours', subtractFactor: [8, 'hour'] },
  { name: 'Last 12 Hours', subtractFactor: [12, 'hour'] },
  { name: 'Last 24 Hours', subtractFactor: [24, 'hour'] },
  { name: 'Last 7 Days', subtractFactor: [7, 'day'] },
  { name: 'Last 14 Days', subtractFactor: [14, 'day'] },
  { name: 'Last 30 Days', subtractFactor: [30, 'day'] },
];
const Header = ({ setFromTime }) => {
  const [currentBucket, setCurrentBucket] = useState(BucketTypes[1]);
  const [dropDownOpen, setDropDownOpen] = useState(false);

  useEffect(() => {
    setFromTime(
      moment()
        .subtract(...currentBucket.subtractFactor)
        .toISOString()
    );
  }, [setFromTime, currentBucket]);

  const reloadData = () =>
    setFromTime(
      moment()
        .subtract(...currentBucket.subtractFactor)
        .toISOString()
    );
  return (
    <div className={`${styles.flex} ${styles.apiHealthHeader}`}>
      <div
        className={`${styles.sm} ${styles.muted} ${styles.pb_xs} ${styles.pl_sm} flex`}
      >
        <Button icon={<FiRefreshCw />} onClick={reloadData}>
          Refresh
        </Button>
        <div className={`${styles.dropdown} ${styles.ml_sm}`}>
          <Button
            icon={<FaChevronDown />}
            iconPosition="end"
            className={`${styles.mr_xs}`}
            onClick={() => setDropDownOpen(b => !b)}
          >
            {currentBucket?.name}
          </Button>
          <div
            className={`${styles.menu} ${dropDownOpen ? styles.active : ''}`}
          >
            {BucketTypes.map(bucket => (
              <li
                key={bucket?.name}
                className={`${styles.link} ${styles.single_line} ${
                  currentBucket.name === bucket.name ? styles.active : ''
                }`}
                onClick={() => {
                  setCurrentBucket(bucket);
                  setDropDownOpen(false);
                }}
              >
                {bucket.name}
              </li>
            ))}
          </div>
        </div>
      </div>
    </div>
  );
};

export default Header;
