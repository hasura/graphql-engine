import React from 'react';

import { StyledRadioButton } from './RadioButton';

export const RadioButton = props => {
  const { children } = props;

  const labelID = String(children);

  return (
    <StyledRadioButton {...props}>
      <input type="radio" id={labelID} name="radio-group" checked />
      <label htmlFor={labelID}>{children}</label>
    </StyledRadioButton>
  );
};
