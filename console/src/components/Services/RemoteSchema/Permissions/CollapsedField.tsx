import React from 'react';
import { FieldType } from './types';
import styles from '../../../Common/Permissions/PermissionStyles.scss';

interface CollapsedFieldProps {
  field: FieldType;
  onClick: (e: React.MouseEvent<HTMLAnchorElement>) => void;
  onExpand: (e: React.MouseEvent<HTMLButtonElement>) => void;
  expanded: boolean;
}
export const CollapsedField: React.FC<CollapsedFieldProps> = ({
  field: i,
  onClick,
  onExpand = () => {},
  expanded,
}) => (
  <>
    {i.return ? (
      <span className={`${styles.padd_small_left} ${styles.fw_medium}`}>
        {i.name}
      </span>
    ) : (
      <button data-test={`field-${i.typeName}`} onClick={onExpand} id={i.name}>
        <span
          className={`${styles.padd_small_left} ${
            expanded ? styles.fw_large : styles.fw_medium
          }`}
        >
          {i.name}
        </span>
      </button>
    )}
    {i.return && (
      <>
        :
        <a
          onClick={onClick}
          id={`${i.return.replace(/[^\w\s]/gi, '')}`}
          href={`${i.return.replace(/[^\w\s]/gi, '')}`}
        >
          {i.return}
        </a>
      </>
    )}
  </>
);
