import React, { useState } from 'react';
import clsx from 'clsx';

/* calls the onChange configured when enter is pressed
 * and clears the current input
 */

import styles from '../../Metrics.module.scss';

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
    <div className={clsx(styles.selectBox)} id={id}>
      <span>{filterTitle}</span>
      <input
        className={clsx(
          styles.commonInput,
          'w-full',
          'block font-normal w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400 placeholder-gray-500'
        )}
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
