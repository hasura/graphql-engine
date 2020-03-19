import React from 'react';

import { StyledCheckBox } from './Checkbox';

export const Checkbox = props => {
  const { children, name } = props;

  return (
    <StyledCheckBox {...props}>
      <input id={name} type="checkbox" value="value1" />
      <label htmlFor={name}>{children}</label>
    </StyledCheckBox>
  );
};
