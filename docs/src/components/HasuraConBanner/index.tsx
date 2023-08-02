import React from 'react';

// import hasuraConDark from '@site/static/img/hasura-con-dark.png';
// import hasuraConLight from '@site/static/img/hasura-con-light.png';
import ArrowRight from '@site/static/icons/arrow_right.svg';

import styles from './styles.module.scss';

const HasuraConBanner = props => {
  const isSnowFlakeSection = props.location.pathname.startsWith(`/docs/latest/databases/snowflake`);

  const isObservabilitySection = props.location.pathname.startsWith(`/docs/latest/observability`);

  const isSecuritySection = props.location.pathname.startsWith(`/docs/latest/security`);

  const isMySQLSection = props.location.pathname.startsWith(`/docs/latest/databases/mysql`);

  const isOracleSection = props.location.pathname.startsWith(`/docs/latest/databases/oracle`);

  const isMariaDBSection = props.location.pathname.startsWith(`/docs/latest/databases/mariadb`);

  // Banner for - New product launch webinar */
  if (isMySQLSection || isOracleSection || isMariaDBSection) {
    return (
      <div className={styles['product-launch-webinar-bg']}>
        <a className={styles['webinar-banner']} href="https://hasura.io/events/webinar/product-launch/">
          <div className={styles['hasura-con-brand']}>
            <img
              className={styles['brand-light']}
              src="https://res.cloudinary.com/dh8fp23nd/image/upload/v1683628053/main-web/Group_11457_vceb9f.png"
              alt="hasura-webinar"
            />
          </div>
          <div className={styles['content-div']}>
            <h3>Ship faster with low-code APIs on MySQL, MariaDB, and Oracle</h3>
            <div className={styles['hasura-con-register'] + ' ' + styles['hasura-con-register-mobile-hide']}>
              View Recording
              <ArrowRight />
            </div>
          </div>
        </a>
      </div>
    );
  }

  if (isSnowFlakeSection) {
    return (
      <div className={styles['snowflake-bg']}>
        <a className={styles['webinar-banner']} href="https://hasura.io/events/webinar/snowflake-and-postgresql/">
          <div className={styles['hasura-con-brand']}>
            <img
              className={styles['brand-light']}
              src="https://res.cloudinary.com/dh8fp23nd/image/upload/v1677756408/main-web/Group_11455_1_ziz1fz.png"
              alt="Hasura Con"
            />
          </div>
          <div className={styles['content-div']}>
            <h3>Combining Snowflake and PostgreSQL to build low-latency apps on historical data insights</h3>
            <div className={styles['hasura-con-register'] + ' ' + styles['hasura-con-register-mobile-hide']}>
              View Recording
              <ArrowRight />
            </div>
          </div>
        </a>
      </div>
    );
  }

  if (isSnowFlakeSection) {
    return (
      <div className={styles['snowflake-bg']}>
        <a className={styles['webinar-banner']} href="https://hasura.io/events/webinar/snowflake-and-postgresql/">
          <div className={styles['hasura-con-brand']}>
            <img
              className={styles['brand-light']}
              src="https://res.cloudinary.com/dh8fp23nd/image/upload/v1677756408/main-web/Group_11455_1_ziz1fz.png"
              alt="Hasura Con"
            />
          </div>
          <div className={styles['content-div']}>
            <h3>Combining Snowflake and PostgreSQL to build low-latency apps on historical data insights</h3>
            <div className={styles['hasura-con-register'] + ' ' + styles['hasura-con-register-mobile-hide']}>
              View Recording
              <ArrowRight />
            </div>
          </div>
        </a>
      </div>
    );
  }

  if (isObservabilitySection) {
    return (
      <div className={styles['observe-bg']}>
        <a
          className={styles['webinar-banner']}
          href="https://hasura.io/events/webinar/best-practices-for-api-observability-with-hasura/"
        >
          <div className={styles['hasura-con-brand']}>
            <img
              className={styles['brand-light']}
              src="https://res.cloudinary.com/dh8fp23nd/image/upload/v1677759444/main-web/Group_11455_2_rdpykm.png"
              alt="Hasura Con"
            />
          </div>
          <div className={styles['content-div']}>
            <h3>Best Practices for API Observability with Hasura</h3>
            <div className={styles['hasura-con-register'] + ' ' + styles['hasura-con-register-mobile-hide']}>
              View Recording
              <ArrowRight />
            </div>
          </div>
        </a>
      </div>
    );
  }

  if (isSecuritySection) {
    return (
      <div className={styles['security-bg']}>
        <a className={styles['webinar-banner']} href="https://hasura.io/events/webinar/securing-your-api-with-hasura/">
          <div className={styles['hasura-con-brand']}>
            <img
              className={styles['brand-light']}
              src="https://res.cloudinary.com/dh8fp23nd/image/upload/v1677759811/main-web/Group_11455_3_azgk7w.png"
              alt="Hasura Con"
            />
          </div>
          <div className={styles['content-div']}>
            <h3>Securing your API with Hasura</h3>
            <div className={styles['hasura-con-register'] + ' ' + styles['hasura-con-register-mobile-hide']}>
              View Recording
              <ArrowRight />
            </div>
          </div>
        </a>
      </div>
    );
  }

  return (
    <a className={styles['hasura-con-banner']} href="https://hasura.io/events/hasura-con-2023/">
      <div className={styles['hasura-con-brand']}>
        <img
          className={styles['hasuracon23-img']}
          src="https://res.cloudinary.com/dh8fp23nd/image/upload/v1686154570/hasura-con-2023/has-con-light-date_r2a2ud.png"
          alt="Hasura Con"
        />
      </div>
      <div className={styles['hasura-con-space-between']}>
        <div>
          <div className={styles['hasura-con-23-title']}>The fourth annual Hasura User Conference</div>
        </div>
        <div className={styles['hasura-con-register-button'] + ' ' + styles['hasura-con-register-mobile-hide']}>
          Read more
          <img
            src="https://res.cloudinary.com/dh8fp23nd/image/upload/v1683723549/main-web/chevron-right_ldbi7d.png"
            alt="arrow-icon"
          />
        </div>
      </div>
    </a>
  );
};

export default HasuraConBanner;
