import React from 'react';
import { FaCircleNotch } from 'react-icons/fa';
import styles from './Common.module.scss';

const LoadingIcon = ({ loading = true }) =>
  loading && <FaCircleNotch className={styles.mar_small_left} />;

export default LoadingIcon;
