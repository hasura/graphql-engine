import React from "react";
import styles from "./PageHelpful.module.css";

import HappyPace from "@site/static/icons/happy.svg";
import SadPace from "@site/static/icons/sad.svg";

const PageHelpful = () => {
  return (
    <div className={styles["helpful-wrapper"]}>
      <h4>Did you find this page helpful?</h4>
      <div className={styles["helpful-icon-wrapper"]}>
        <div className={styles["helpful-icon"] + " " + styles["active"]}>
          <HappyPace />
        </div>
        <div className={styles["helpful-icon"]}>
          <SadPace />
        </div>
      </div>
    </div>
  )
}

export default PageHelpful;
