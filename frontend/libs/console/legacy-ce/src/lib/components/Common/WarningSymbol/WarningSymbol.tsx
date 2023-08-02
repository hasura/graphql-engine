import React from 'react';
import { Tooltip } from '../../../new-components/Tooltip';
import { FaExclamationTriangle } from 'react-icons/fa';

import styles from './WarningSymbol.module.scss';

export interface WarningSymbolProps {
  tooltipText: string;
  tooltipPlacement?: 'left' | 'right' | 'top' | 'bottom';
  customStyle?: string;
}

const WarningSymbol: React.FC<WarningSymbolProps> = ({
  tooltipText,
  tooltipPlacement = 'right',
  customStyle = '',
}) => {
  return (
    <div className={styles.display_inline}>
      <Tooltip tooltipContentChildren={tooltipText} side={tooltipPlacement}>
        <span>
          <WarningIcon customStyle={customStyle} />
        </span>
      </Tooltip>
    </div>
  );
};

export interface WarningIconProps {
  customStyle?: string;
}

export const WarningIcon: React.FC<WarningIconProps> = ({
  customStyle = '',
}) => {
  return (
    <FaExclamationTriangle
      className={`${styles.warningSymbol} ${customStyle}`}
      aria-hidden="true"
    />
  );
};

export default WarningSymbol;
