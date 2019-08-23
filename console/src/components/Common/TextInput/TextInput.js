import React from 'react';
import PropTypes from 'prop-types';

const TextInput = props => {
  const { type = 'text', placeholder = '', bsclass = null, onChange } = props;
  return (
    <input
      {...props}
      type={type}
      placeholder={placeholder}
      className={`${bsclass} form-control`}
      onChange={onChange}
    />
  );
};

TextInput.propTypes = {
  onChange: PropTypes.func.isRequired,
};

export default TextInput;
