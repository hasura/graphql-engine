/* eslint-disable jsx-a11y/no-noninteractive-element-interactions */
import React from 'react';

import Button from '../../Common/Button/Button';
import close from '../images/x-circle.svg';
import monitoring from '../images/monitoring.svg';
import rate from '../images/rate.svg';
import regression from '../images/regression.svg';
import management from '../images/management.svg';
import allow from '../images/allow-listing.svg';
import dataCaching from '../images/data-caching.svg';
import styles from '../Main.scss';

type ProPopupProps = {
  toggleOpen: () => void;
};
export const ProPopup: React.FC<ProPopupProps> = ({ toggleOpen }) => (
  <div className={styles.proPopUpWrapper}>
    <div className={styles.popUpHeader}>
      Hasura <span>CLOUD</span>
      <img
        onClick={toggleOpen}
        className={styles.popUpClose}
        src={close}
        alt="Close"
      />
    </div>
    <div className={styles.popUpBodyWrapper}>
      <div className={styles.featuresDescription}>
        Hasura Cloud gives you a scalable, highly available, globally
        distributed, fully managed, secure GraphQL API as a service!
      </div>
      <div className={styles.proFeaturesList}>
        <div className={styles.featuresImg}>
          <img src={monitoring} alt="Monitoring" />
        </div>
        <div className={styles.featuresList}>
          <div className={styles.featuresTitle}>Monitoring/Analytics</div>
          <div className={styles.featuresDescription}>
            Complete observability: Troubleshoot errors & drill-down into
            individual operations.
          </div>
        </div>
      </div>
      <div className={styles.proFeaturesList}>
        <div className={styles.featuresImg}>
          <img src={rate} alt="Rate" />
        </div>
        <div className={styles.featuresList}>
          <div className={styles.featuresTitle}>Rate Limiting</div>
          <div className={styles.featuresDescription}>
            Role-based rate limits to prevent abuse.
          </div>
        </div>
      </div>
      <div className={styles.proFeaturesList}>
        <div className={styles.featuresImg}>
          <img src={regression} alt="Regression" />
        </div>
        <div className={styles.featuresList}>
          <div className={styles.featuresTitle}>Regression Testing</div>
          <div className={styles.featuresDescription}>
            Automatically create regression suites to prevent breaking changes.
          </div>
        </div>
      </div>
      <div className={styles.proFeaturesList}>
        <div className={styles.featuresImg}>
          <img src={management} alt="Management" />
        </div>
        <div className={styles.featuresList}>
          <div className={styles.featuresTitle}>Team Management</div>
          <div className={styles.featuresDescription}>
            Login to Hasura project with granular privileges.
          </div>
        </div>
      </div>
      <div className={styles.proFeaturesList}>
        <div className={styles.featuresImg}>
          <img src={dataCaching} alt="data caching" />
        </div>
        <div className={styles.featuresList}>
          <div className={styles.featuresTitle}>
            Query & Dynamic Data caching
          </div>
          <div className={styles.featuresDescription}>
            Automatic caching of your shared data, cache query plans at the
            GraphQL and at the database level and blazing fast performance.
          </div>
        </div>
      </div>
      <div className={styles.proFeaturesList}>
        <div className={styles.featuresImg}>
          <img src={allow} alt="allow" />
        </div>
        <div className={styles.featuresList}>
          <div className={styles.featuresTitle}>Allow Listing</div>
          <div className={styles.featuresDescription}>
            Allow listing workflows across dev, staging and production
            environments.
          </div>
        </div>
      </div>
    </div>
    <div className={styles.popUpFooter}>
      <a
        href="https://cloud.hasura.io/"
        target="_blank"
        rel="noopener noreferrer"
      >
        <Button
          data-test="data-get-started"
          color="yellow"
          className={styles.largeButton}
        >
          Get started
        </Button>
      </a>
    </div>
  </div>
);
