import React from 'react';

import { StyledCheckBox } from './Checkbox';

export const Checkbox = props => {
  const { children } = props;

  const labelID = String(children);

  return (
    <StyledCheckBox {...props}>
      <input id={labelID} type="checkbox" value="value1" />
      <label htmlFor={labelID}>{children}</label>
    </StyledCheckBox>
  );
};
