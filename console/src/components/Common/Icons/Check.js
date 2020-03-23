import React from 'react';

import { Icon } from '../../UIKit/atoms';

const Check = ({ className }) => {
  return (
    <Icon
      type="check"
      size={20}
      color="green.original"
      className={className || ''}
    />
  );
};

export default Check;
