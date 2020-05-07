import React, { useState } from 'react';

import {
  StyledSwitchButton,
  StyledSlider,
  StyledSwitchButtonProps,
} from './SwitchButton';

interface SwitchButtonProps extends StyledSwitchButtonProps {}

export const SwitchButton: React.FC<SwitchButtonProps> = props => {
  const [isChecked, toggleCheckbox] = useState(false);
  const { children } = props;

  return (
    <StyledSwitchButton {...props}>
      <label>
        <input type="checkbox" onClick={() => toggleCheckbox(!isChecked)} />
        <StyledSlider checked={isChecked} />
        {children}
      </label>
    </StyledSwitchButton>
  );
};
