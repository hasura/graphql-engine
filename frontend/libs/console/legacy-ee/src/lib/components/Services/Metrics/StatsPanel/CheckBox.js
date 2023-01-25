import React from 'react';

/* calls the onChange configured when enter is pressed
 * and clears the current input
 */

import styles from '../Metrics.module.scss';

const CheckBox = props => {
  const { onChange, id, title, value, checked } = props;
  return (
    <div id={id} className={styles.selectBox + ' ' + styles.commonCheckBox}>
      <input
        id={id}
        type="checkbox"
        checked={checked}
        className="legacy-input-fix"
      />
      <label
        className={styles.fontWeightBold}
        data-field-value={value}
        onClick={onChange}
        htmlFor={id}
      >
        {title}
      </label>
    </div>
  );
};
export default CheckBox;
