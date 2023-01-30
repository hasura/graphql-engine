import React from 'react';

import CheckBox from '../CheckBox';
import styles from '../../Metrics.module.scss';

const GenerateGroupBy = ({ id, title, value, onChange, checked }) => {
  const onCheckBoxClick = e => {
    const val = e.target.getAttribute('data-field-value');
    onChange(val);
  };
  return (
    <CheckBox
      id={id}
      onChange={onCheckBoxClick}
      title={title}
      value={value}
      checked={checked}
    />
  );
};
const GenerateGroupBys = ({ getTitle, values, selected, onChange }) => {
  const groupBy = values.map((v, i) => (
    <GenerateGroupBy
      key={i}
      id={`groupBy-${v}`}
      title={getTitle(v)}
      value={v}
      onChange={onChange}
      checked={selected.indexOf(v) !== -1}
    />
  ));
  return (
    <div className={styles.filterSectionWrapper}>
      <div className={styles.selectBoxWrapper}>{groupBy}</div>
    </div>
  );
};

export default GenerateGroupBys;
