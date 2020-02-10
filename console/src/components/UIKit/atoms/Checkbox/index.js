import React from 'react';

import { CheckboxStyles } from './Checkbox.style';

const Checkbox = props => {
  const { children } = props;

  return (
    <CheckboxStyles {...props}>
      <input id={children} type="checkbox" value="value1" />
      <label htmlFor={children}>{children}</label>
    </CheckboxStyles>
  );
};

export default Checkbox;
