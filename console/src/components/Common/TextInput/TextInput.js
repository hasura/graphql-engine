import React from 'react';
import PropTypes from 'prop-types';

const TextInput = props => {
  const {
    testid,
    type = 'text',
    placeholder = '',
    bsclass = null,
    onChange,
  } = props;
  return (
    <input
      {...props}
      type={type}
      data-test={testid}
      placeholder={placeholder}
      className={`${bsclass} form-control`}
      onChange={onChange}
    />
  );
};

TextInput.propTypes = {
  onChange: PropTypes.func.isRequired,
  testid: PropTypes.string.isRequired,
};

export default TextInput;
