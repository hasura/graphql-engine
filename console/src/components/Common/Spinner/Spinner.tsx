import React from 'react';
import styles from './Spinner.scss';

type Props = {
  className?: string;
};

const Spinner: React.FC<Props> = ({ className = '' }) => {
  return (
    <div className={`${styles.sk_circle} ${className}`}>
      <div className={`${styles.sk_circle1} ${styles.sk_child}`} />
      <div className={`${styles.sk_circle2} ${styles.sk_child}`} />
      <div className={`${styles.sk_circle3} ${styles.sk_child}`} />
      <div className={`${styles.sk_circle4} ${styles.sk_child}`} />
      <div className={`${styles.sk_circle5} ${styles.sk_child}`} />
      <div className={`${styles.sk_circle6} ${styles.sk_child}`} />
      <div className={`${styles.sk_circle7} ${styles.sk_child}`} />
      <div className={`${styles.sk_circle8} ${styles.sk_child}`} />
      <div className={`${styles.sk_circle9} ${styles.sk_child}`} />
      <div className={`${styles.sk_circle10} ${styles.sk_child}`} />
      <div className={`${styles.sk_circle11} ${styles.sk_child}`} />
      <div className={`${styles.sk_circle12} ${styles.sk_child}`} />
    </div>
  );
};

export default Spinner;
