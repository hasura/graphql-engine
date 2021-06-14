import React from 'react';
import styles from './PermissionStyles.scss';

export const permissionsSymbols = {
  fullAccess: (
    <i
      className={`fa fa-check ${styles.permissionSymbolFA}`}
      aria-hidden="true"
    />
  ),
  noAccess: (
    <i
      className={`fa fa-times ${styles.permissionSymbolNA}`}
      aria-hidden="true"
    />
  ),
  partialAccess: (
    <i
      className={`fa fa-filter ${styles.permissionSymbolPA}`}
      aria-hidden="true"
    />
  ),
  partialAccessWarning: (
    <i
      className={`fa fa-exclamation-triangle ${styles.permissionSymbolPAW}`}
      aria-hidden="true"
    />
  ),
};
