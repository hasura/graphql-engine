import React from 'react';

import { Icon } from '../../UIKit/atoms';

const Cross = ({ className }) => {
  return (
    <Icon
      type="close"
      color="red.primary"
      size={20}
      className={className || ''}
    />
  );
};

export default Cross;
