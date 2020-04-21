import React, { useState } from 'react';

import { StyledSwitchButton, StyledSlider } from './SwitchButton';

export const SwitchButton: React.FC = props => {
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
