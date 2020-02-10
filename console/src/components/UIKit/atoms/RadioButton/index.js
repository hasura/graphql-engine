import React from 'react';

import { RadioButtonStyles } from './RadioButton.style';

const RadioBtn = props => {
  const { children } = props;

  return (
    <RadioButtonStyles {...props}>
      <input type="radio" id={children} name="radio-group" checked />
      <label htmlFor={children}>{children}</label>
    </RadioButtonStyles>
  );
};

export default RadioBtn;
