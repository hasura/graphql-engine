import React, { Fragment, useState } from "react";
import { saTrack } from "@site/src/utils/segmentAnalytics";
import HappyFace from "@site/static/icons/happy.svg";
import SadFace from "@site/static/icons/sad.svg";
import styles from "./styles.module.scss";

const PageHelpful = () => {
  const [isHelpful, setIsHelpful] = useState<boolean>();
  const [hasResponse, setHasResponse] = useState<boolean>(false);

  const recordResponse = helpful => {
    if(hasResponse) return;

    setIsHelpful(helpful);
    setHasResponse(true);

    saTrack("Responded to Did You Find This Page Helpful", {
      label: "Responded to Did You Find This Page Helpful",
      response: helpful ? "YES" : "NO",
      pageUrl: window.location.href,
    });
  }

  return (
    <div className={styles["helpful-wrapper"]}>
      <h4>Did you find this page helpful?</h4>
      <div className={styles["helpful-icon-wrapper"]}>
        <div
          className={`${styles["helpful-icon"]} ${
            hasResponse ? isHelpful ? styles.active : "" : styles.no_response
          }`}
          onClick={() => recordResponse(true)}
        >
          <HappyFace />
        </div>
        <div
          className={`${styles["helpful-icon"]} ${
            hasResponse ? !isHelpful ? styles.active : "" : styles.no_response
          }`}
          onClick={() => recordResponse(false)}
        >
          <SadFace />
        </div>
      </div>
      {hasResponse && <div>Thank you for your feedback!</div>}
    </div>
  );
};

export default PageHelpful;
