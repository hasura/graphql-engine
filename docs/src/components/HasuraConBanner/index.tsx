import React from 'react';

import hasuraConDark from "@site/static/img/hasura-con-dark.png";
import hasuraConLight from "@site/static/img/hasura-con-light.png";
import ArrowRight from "@site/static/icons/arrow_right.svg";

import styles from './styles.module.scss';

const HasuraConBanner = (props) => {
  return (
    <a className={styles["hasura-con-banner"]} href="https://hasura.io/events/hasura-con-2022/">
      <div className={styles["hasura-con-brand"]}>
        <img className={styles["brand-dark"]} src={hasuraConDark} alt="Hasura Con" />
        <img className={styles["brand-light"]} src={hasuraConLight} alt="Hasura Con" />
        <div>June 28 - 30, 2022</div>
      </div>
      <div className={styles["hasura-con-space-between"]}>
      <div>
        <div className={styles["hasura-con-desc"]}>
          The Hasura User Conference
        </div>
        <div className={styles["hasura-con-title"]}>
          GraphQL for Everyone
        </div>
      </div>
      <div className={styles["hasura-con-register"] + " " + styles["hasura-con-register-mobile-hide"]}>
        Register Now
        <ArrowRight />
      </div>
      </div>
    </a>
  );
}

export default HasuraConBanner;
