import React from 'react';

import { Icon } from '../../UIKit/atoms';

export const permissionsSymbols = {
  fullAccess: <Icon color="green.primary" type="check" size={16} />,
  noAccess: <Icon type="close" color="red.original" size={16} />,
  partialAccess: <Icon type="filter" size={16} />,
};
