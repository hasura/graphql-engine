import React from 'react';

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
      <i
        className={styles.padd_right + ' fa fa-info-circle'}
        aria-hidden="true"
      />
      {suggestionText}
    </div>
  ) : (
    ''
  );
};

export default generateSuggestionBox;
