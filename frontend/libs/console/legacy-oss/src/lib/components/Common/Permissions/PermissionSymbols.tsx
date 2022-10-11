import React from 'react';
import {
  FaCheck,
  FaTimes,
  FaFilter,
  FaExclamationTriangle,
} from 'react-icons/fa';
import styles from './PermissionStyles.module.scss';

export const permissionsSymbols = {
  fullAccess: (
    <FaCheck className={`${styles.permissionSymbolFA}`} aria-hidden="true" />
  ),
  noAccess: (
    <FaTimes className={`${styles.permissionSymbolNA}`} aria-hidden="true" />
  ),
  partialAccess: (
    <FaFilter className={`${styles.permissionSymbolPA}`} aria-hidden="true" />
  ),
  partialAccessWarning: (
    <FaExclamationTriangle
      className={`${styles.permissionSymbolPAW}`}
      aria-hidden="true"
    />
  ),
};
