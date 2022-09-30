import React, { useState } from 'react';

/* calls the onChange configured when enter is pressed
 * and clears the current input
 */

const styles = require('../../Metrics.scss');

const FilterInputComponent = props => {
  const [value, updateInput] = useState('');
  const { onChange, placeholder, id, filterTitle } = props;
  const handleEnterKeyPress = e => {
    if (e.keyCode === 13) {
      const val = e.target.value;
      if (val && val.length > 0) {
        updateInput('');
        onChange(e.target.value);
      }
    }
  };
  return (
    <div className={styles.selectBox} id={id}>
      <span>{filterTitle}</span>
      <input
        className={styles.commonInput}
        type="text"
        placeholder={placeholder}
        onKeyDown={handleEnterKeyPress}
        value={value}
        onChange={e => updateInput(e.target.value)}
      />
    </div>
  );
};
export default FilterInputComponent;
