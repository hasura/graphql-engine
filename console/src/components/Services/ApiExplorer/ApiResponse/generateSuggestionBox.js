import React from 'react';

import { Icon } from '../../../UIKit/atoms';
import styles from '../ApiExplorer.scss';

const generateSuggestionBox = (response, parseFunc) => {
  const suggestionText = parseFunc(response);
  return suggestionText ? (
    <div
      style={{ marginBottom: '0px', display: 'flex' }}
      className={
        styles.clear_fix + ' ' + styles.alertDanger + ' alert alert-danger'
      }
    >
      <Icon type="info" className={styles.padd_right} />
      {suggestionText}
    </div>
  ) : (
    ''
  );
};

export default generateSuggestionBox;
