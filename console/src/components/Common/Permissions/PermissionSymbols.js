import React from 'react';

import { Icon } from '../../UIKit/atoms';

export const permissionsSymbols = {
  fullAccess: <Icon color="green.original" type="check" size={16} pt="6px" />,
  noAccess: <Icon type="close" color="red.primary" size={16} pt="xs" />,
  partialAccess: <Icon type="filter" size={16} pt="6px" />,
};
