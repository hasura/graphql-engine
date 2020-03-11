import React from 'react';

import { StyledRadioButton } from './RadioButton';

export const RadioButton = props => {
  const { children } = props;

  return (
    <StyledRadioButton {...props}>
      <input type="radio" id={children} name="radio-group" checked />
      <label htmlFor={children}>{children}</label>
    </StyledRadioButton>
  );
};
