import React from "react";
import LogoDark from "@site/static/img/logo.svg";
import LogoLight from "@site/static/img/logo-light.svg";
import BrowserOnly from "@docusaurus/BrowserOnly";
import styles from "./CustomFooter.module.scss";

const CustomFooter = () => {
  return (
    <footer className={styles["custom-footer-wrapper"]}>
      <div>
        <LogoLight className={styles["dark-theme-logo"]} />
        <LogoDark className={styles["light-theme-logo"]} />
      </div>
      <div className={""}>
        {`Copyright © ${new Date().getFullYear()} Hasura Inc. All rights reserved`}
      </div>
      <div className={""}>
        {/* < /> */}
      </div>
    </footer>
  )
}

export default CustomFooter;
