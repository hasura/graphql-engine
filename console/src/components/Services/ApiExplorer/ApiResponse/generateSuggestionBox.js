import React from 'react';
import { FaInfoCircle } from 'react-icons/fa';

const styles = require('../ApiExplorer.scss');

const generateSuggestionBox = (response, parseFunc) => {
  const suggestionText = parseFunc(response);
  return suggestionText ? (
    <div
      style={{ marginBottom: '0px', display: 'flex' }}
      className={
        styles.clear_fix + ' ' + styles.alertDanger + ' alert alert-danger'
      }
    >
      <FaInfoCircle className={styles.padd_right} aria-hidden="true" />
      {suggestionText}
    </div>
  ) : (
    ''
  );
};

export default generateSuggestionBox;
