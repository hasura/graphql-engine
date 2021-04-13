import React from 'react';
import styles from '../../../Common/TableCommon/Table.scss';

interface Props {
  options: string[];
  defaultValue: string | '';
  onChange: (option: string) => void;
  styles: React.CSSProperties;
}

const DropDownSelector: React.FC<Props> = ({
  options,
  defaultValue,
  onChange,
  ...props
}) => {
  const handleValueChange = (event: React.ChangeEvent<HTMLSelectElement>) => {
    event.persist();
    onChange(event.target.value);
  };

  return (
    <div style={props.styles}>
      <select
        className={`${styles.select} ${styles.sample} form-control ${styles.add_pad_left}`}
        name="data-source"
        onChange={handleValueChange}
        defaultValue={defaultValue}
      >
        {options.map((item: string, index: number) => (
          <option key={index}>{item}</option>
        ))}
      </select>
    </div>
  );
};

export default DropDownSelector;
