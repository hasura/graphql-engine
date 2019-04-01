import React, { useState } from 'react';
import PropTypes from 'prop-types';
import TextInput from '../TextInput/TextInput';

const SuggestionBox = props => {
  const {
    inputPlaceholder = 'column_type',
    children,
    // testid,
    // onChange
  } = props;
  const [isActive, toggle] = useState(false);
  const styles = require('./SuggestionBox.scss');
  const suggestionNode = isActive ? (
    <div className={styles.suggestion_wrapper}>{children}</div>
  ) : null;
  return (
    <div className={styles.suggestion_box_wrapper}>
      <TextInput
        placeholder={inputPlaceholder}
        bsclass={styles.suggestion_input}
        onFocus={() => toggle(!isActive)}
        onBlur={() => toggle(!isActive)}
      />
      {suggestionNode}
    </div>
  );
};

SuggestionBox.propTypes = {
  inputPlaceholder: PropTypes.string,
  children: PropTypes.node.isRequired,
};

export default SuggestionBox;
