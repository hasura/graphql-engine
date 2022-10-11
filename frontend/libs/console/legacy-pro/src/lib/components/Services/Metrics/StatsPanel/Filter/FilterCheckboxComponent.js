import React from 'react';

/* calls the onChange configured when enter is pressed
 * and clears the current input
 */

const styles = require('../../Metrics.scss');

const FilterCheckboxComponent = props => {
  const { onChange, id, title, checked, bsClass } = props;
  const customOnChange = e => {
    const val = e.target.getAttribute('data-field-value');
    onChange(val);
  };
  return (
    <div
      id={id}
      className={styles.selectBox + ' ' + styles.commonCheckBox + ' ' + bsClass}
    >
      <input
        id={id}
        type="checkbox"
        checked={checked}
        onChange={() => null}
        className="legacy-input-fix"
      />
      <label
        className={styles.fontWeightBold}
        data-field-value={id}
        onClick={customOnChange}
        htmlFor={id}
      >
        {title}
      </label>
    </div>
  );
};
export default FilterCheckboxComponent;
