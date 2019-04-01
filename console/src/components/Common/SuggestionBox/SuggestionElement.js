import React from 'react';

const styles = require('./SuggestionBox.scss');

const SuggestionElement = ({ value }) => {
  return (
    <div className={styles.suggestion_element_wrapper}>
      <span className={styles.suggestion_element}>{value}</span>
    </div>
  );
};

const SuggestionGenerator = ({ values }) => {
  return (
    <div className={styles.suggestion_generator}>
      {values.map((v, i) => (
        <SuggestionElement key={i} value={v} />
      ))}
    </div>
  );
};

export { SuggestionElement, SuggestionGenerator };
