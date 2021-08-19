import React from 'react';

import styles from './PermissionBuilder.scss';
import { addToPrefix } from './utils';

type OptGroup = { optGroupTitle: string; options: string[] };
interface SelectGroupProps {
  selectDispatchFunc: (value: string) => void;
  value: string;
  values: OptGroup[];
  prefix?: string;
  disabledValues?: string[];
}

const optGroupSortFn = (a: OptGroup, b: OptGroup) => {
  if (a.optGroupTitle === 'root') return 1;
  if (b.optGroupTitle === 'root') return -1;
  return 0;
};

const SelectGroup: React.FC<SelectGroupProps> = ({
  selectDispatchFunc,
  value,
  values,
  prefix = '',
  disabledValues = [],
}) => {
  const dispatchSelect = (e: React.ChangeEvent<HTMLSelectElement>) => {
    selectDispatchFunc(e.target.value);
  };
  const selectOptions = [];

  selectOptions.push(
    <option value={addToPrefix(prefix, '--')} key={0} disabled>
      --
    </option>
  );

  values.sort(optGroupSortFn).forEach(({ optGroupTitle, options }, i) => {
    if (options?.length) {
      selectOptions.push(
        <optgroup label={optGroupTitle} key={i + 1}>
          {options.map((option, j) => (
            <option
              value={addToPrefix(prefix, option)}
              key={j}
              disabled={disabledValues.includes(option)}
            >
              {option || '--'}
            </option>
          ))}
        </optgroup>
      );
    }
  });
  const selectedValue = addToPrefix(prefix, value || '--');

  return (
    <select
      value={selectedValue}
      name={prefix}
      onChange={dispatchSelect}
      className={styles.qb_select}
      data-test="qb-select"
    >
      {selectOptions}
    </select>
  );
};

export default SelectGroup;

export const QuotedSelectGroup: React.FC<SelectGroupProps> = props => {
  return (
    <span>
      &quot;&nbsp;
      <SelectGroup {...props} />
      &quot;&nbsp;
    </span>
  );
};
