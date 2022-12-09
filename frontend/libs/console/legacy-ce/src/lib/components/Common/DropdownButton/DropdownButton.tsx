import React from 'react';
import InputGroup from 'react-bootstrap/lib/InputGroup';
import DropdownButton from 'react-bootstrap/lib/DropdownButton';
import MenuItem from 'react-bootstrap/lib/MenuItem';

type DropDownButtonProps = {
  title: string;
  dropdownOptions: {
    display_text: string;
    value: string;
  }[];
  dataKey: string;
  dataIndex?: string;
  onButtonChange: (e: React.MouseEvent<unknown>) => void;
  onInputChange: (e: React.ChangeEvent<HTMLInputElement>) => void;
  value?: string;
  inputVal: string;
  required: boolean;
  id?: string;
  testId?: string;
  disabled?: boolean;
  bsClass?: string;
  inputPlaceHolder: string;
  inputStyle?: Record<string, string>;
};

const DDButton: React.FC<DropDownButtonProps> = props => {
  const {
    title,
    dropdownOptions,
    value,
    required,
    onInputChange,
    onButtonChange,
    dataKey,
    dataIndex,
    bsClass,
    disabled,
    inputVal,
    inputPlaceHolder,
    id,
    testId,
  } = props;
  return (
    <InputGroup className={bsClass}>
      <DropdownButton
        title={value || title}
        componentClass={InputGroup.Button}
        disabled={disabled}
        id={id || ''}
        data-test={`${testId}-dropdown-button`}
      >
        {dropdownOptions.map((d, i) => (
          <MenuItem
            data-index-id={dataIndex}
            value={d.value}
            onClick={onButtonChange}
            eventKey={i + 1}
            key={i}
            data-test={`${testId}-dropdown-item-${i + 1}`}
          >
            {d.display_text}
          </MenuItem>
        ))}
      </DropdownButton>
      <input
        style={props.inputStyle}
        type="text"
        data-key={dataKey}
        data-index-id={dataIndex}
        className="form-control"
        required={required}
        onChange={onInputChange}
        disabled={disabled}
        value={inputVal || ''}
        placeholder={inputPlaceHolder}
        data-test={`${testId}-input`}
      />
    </InputGroup>
  );
};

export default DDButton;
