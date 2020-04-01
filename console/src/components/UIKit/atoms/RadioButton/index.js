import React from 'react';

import { StyledRadioButton } from './RadioButton';

export const RadioButton = props => {
  const { children, name } = props;

  return (
    <StyledRadioButton {...props}>
      <input type="radio" id={name} name="radio-group" checked />
      <label htmlFor={name}>{children}</label>
    </StyledRadioButton>
  );
};
