import React from 'react';
import styles from './Spinner.module.scss';

type Size = {
  height?: string;
  width?: string;
};

const getStyle = ({ height, width }: Size) => {
  const baseStyle: Size = {};
  if (height) {
    baseStyle.height = height;
  }

  if (width) {
    baseStyle.width = width;
  }
  return baseStyle;
};

type Props = {
  className?: string;
  width?: string;
  height?: string;
};

const Spinner: React.FC<Props> = ({
  className = '',
  width = '16px',
  height = '16px',
}) => {
  const style = getStyle({ width, height });
  return (
    <div className={`${styles.sk_circle} ${className}`} style={style}>
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
