import React from 'react';
import { FieldType } from './types';
import styles from '../../../Common/Permissions/PermissionStyles.scss';

interface CollapsedFieldProps {
  field: FieldType;
  onClick: (e: React.MouseEvent<HTMLAnchorElement>) => void;
  onExpand: (e: React.MouseEvent<HTMLButtonElement>) => void;
}
export const CollapsedField: React.FC<CollapsedFieldProps> = ({
  field: i,
  onClick,
  onExpand = () => {},
}) => (
  <>
    <button onClick={onExpand} id={i.name}>
      <b className={styles.padd_small_left}>{i.name}</b>
    </button>
    {i.return && (
      <b>
        :
        <a
          onClick={onClick}
          id={`${i.return.replace(/[^\w\s]/gi, '')}`}
          href={`${i.return.replace(/[^\w\s]/gi, '')}`}
        >
          {i.return}
        </a>
      </b>
    )}
  </>
);
