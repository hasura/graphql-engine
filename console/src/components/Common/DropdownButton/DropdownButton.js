import React from 'react';
import PropTypes from 'prop-types';

import InputGroup from 'react-bootstrap/lib/InputGroup';
import DropdownButton from 'react-bootstrap/lib/DropdownButton';
import MenuItem from 'react-bootstrap/lib/MenuItem';

const DropButton = props => {
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
        id={id}
        data-test={testId + '-' + 'dropdown-button'}
      >
        {dropdownOptions.map((d, i) => (
          <MenuItem
            data-index-id={dataIndex}
            value={d.value}
            onClick={onButtonChange}
            eventKey={i + 1}
            key={i}
            data-test={testId + '-' + 'dropdown-item' + '-' + (i + 1)}
          >
            {d.display_text}
          </MenuItem>
        ))}
      </DropdownButton>
      <input
        type="text"
        data-key={dataKey}
        data-index-id={dataIndex}
        className={'form-control'}
        required={required}
        onChange={onInputChange}
        disabled={disabled}
        value={inputVal || ''}
        placeholder={inputPlaceHolder}
        data-test={testId + '-' + 'input'}
      />
    </InputGroup>
  );
};

DropButton.propTypes = {
  dispatch: PropTypes.func.isRequired,
  dropdownOptions: PropTypes.array.isRequired,
  title: PropTypes.string.isRequired,
  value: PropTypes.string.isRequired,
  dataKey: PropTypes.string.isRequired,
  dataIndex: PropTypes.string.isRequired,
  inputVal: PropTypes.string.isRequired,
  inputPlaceHolder: PropTypes.string,
  required: PropTypes.bool.isRequired,
  onButtonChange: PropTypes.func.isRequired,
  onInputChange: PropTypes.func.isRequired,
  bsClass: PropTypes.string,
  id: PropTypes.string,
  testId: PropTypes.string,
  disabled: PropTypes.bool.isRequired,
};

export default DropButton;
