import React from 'react';

import { StyledCheckBox } from './Checkbox';

export const Checkbox = props => {
  const { children } = props;

  return (
    <StyledCheckBox {...props}>
      <input id={children} type="checkbox" value="value1" />
      <label htmlFor={children}>{children}</label>
    </StyledCheckBox>
  );
};
