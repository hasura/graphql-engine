import React from 'react';

import { Icon } from '../../UIKit/atoms';
import styles from './PermissionStyles.scss';

export const permissionsSymbols = {
  fullAccess: <Icon className={styles.permissionSymbolFA} type="check" />,
  noAccess: <Icon type="close" className={styles.permissionSymbolNA} />,
  partialAccess: <Icon type="filter" className={styles.permissionSymbolPA} />,
};
