import React from 'react';

import { StyledCheckBox } from './Checkbox';

export type CheckBoxProps = {
  name: string;
};

export const Checkbox: React.FC<CheckBoxProps> = props => {
  const { children, name } = props;

  return (
    <StyledCheckBox {...props}>
      <input id={name} type="checkbox" value="value1" />
      <label htmlFor={name}>{children}</label>
    </StyledCheckBox>
  );
};
