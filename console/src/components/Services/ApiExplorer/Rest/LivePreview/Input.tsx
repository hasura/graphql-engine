import React from 'react';

import styles from '../RESTStyles.scss';

const Input: React.FC<React.ComponentProps<'input'>> = props => (
  <input className={styles.rest_preview_input} {...props} />
);

export default Input;
