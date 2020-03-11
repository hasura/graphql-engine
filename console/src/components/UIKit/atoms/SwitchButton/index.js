import React from 'react';

import { StyledSwitchButton } from './SwitchButton';

export const SwitchButton = props => {
  const { children } = props;

  return (
    <StyledSwitchButton {...props}>
      <label>
        <input type="checkbox" />
        <span className={'slider' + ' ' + 'round'} />
        {children}
      </label>
    </StyledSwitchButton>
  );
};
