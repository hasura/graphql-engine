import React from 'react';

import { Icon } from '../../UIKit/atoms';

const RemoveIcon = ({ className }) => (
  <Icon type="close" className={className || ''} ml="10px" size={15} />
);

export default RemoveIcon;
