import styles from "./styles.module.scss";
import Hand from "@site/static/img/mascot-hand.png";
import React from "react";

export const ScrollToFeedbackButton = () => {

  const scrollToFeedback = () => {
    const feedbackElement = document.getElementById('feedback');
    const y = feedbackElement.getBoundingClientRect().top + window.scrollY - 100;
    window.scrollTo({top: y, behavior: 'smooth'});
  }

  return (
    <div className={styles.scrollToWrapper} onClick={scrollToFeedback}>
      Feedback 👋
    </div>
  )
}