import React from 'react';

import { SwitchButtonStyles } from './SwitchButton.style';

const SwitchButton = props => {
  const { children } = props;

  return (
    <SwitchButtonStyles>
      <input type="checkbox" />
      <span className={'slider' + ' ' + 'round'} />
      {children}
    </SwitchButtonStyles>
  );
};

export default SwitchButton;
