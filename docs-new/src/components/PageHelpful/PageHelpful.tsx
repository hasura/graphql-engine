import React from "react";
import styles from "./PageHelpful.module.scss";

import HappyFace from "@site/static/icons/happy.svg";
import SadFace from "@site/static/icons/sad.svg";

const PageHelpful = () => {
  return (
    <div className={styles["helpful-wrapper"]}>
      <h4>Did you find this page helpful?</h4>
      <div className={styles["helpful-icon-wrapper"]}>
        <div className={styles["helpful-icon"] + " " + styles["active"]}>
          <HappyFace />
        </div>
        <div className={styles["helpful-icon"]}>
          <SadFace />
        </div>
      </div>
    </div>
  )
}

export default PageHelpful;
