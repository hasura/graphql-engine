import React from 'react';
import { FaExclamationTriangle } from 'react-icons/fa';

import { Tooltip } from '../Tooltip';
import { IconButton } from '../Button';

export interface WarningSymbolProps {
  tooltipText: string;
  tooltipPlacement?: 'left' | 'right' | 'top' | 'bottom';
  customStyle?: string;
}

export const WarningSymbol: React.FC<WarningSymbolProps> = ({
  tooltipText,
  tooltipPlacement = 'right',
  customStyle = '',
}) => {
  return (
    <div className="inline-block">
      <Tooltip content={tooltipText} side={tooltipPlacement}>
        <span>
          <WarningIcon customStyle={customStyle} />
        </span>
      </Tooltip>
    </div>
  );
};

interface WarningIconProps {
  customStyle?: string;
}

const WarningIcon: React.FC<WarningIconProps> = ({ customStyle = '' }) => {
  return (
    <IconButton color="red" variant="ghost">
      <FaExclamationTriangle className={customStyle} aria-hidden="true" />
    </IconButton>
  );
};
