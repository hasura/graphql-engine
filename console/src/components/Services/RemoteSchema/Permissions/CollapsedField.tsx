import React from 'react';
import { FieldType } from './types';

interface CollapsedFieldProps {
  field: FieldType;
  onClick: (e: React.MouseEvent<HTMLAnchorElement>) => void;
}
export const CollapsedField: React.FC<CollapsedFieldProps> = ({
  field: i,
  onClick,
}) => (
  <>
    <b id={i.name}>{i.name}</b>
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
