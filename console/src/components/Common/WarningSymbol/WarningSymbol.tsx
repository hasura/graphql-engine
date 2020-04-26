import React from 'react';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';

import styles from './WarningSymbol.scss';

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
  const tooltip = <Tooltip id={tooltipText}>{tooltipText}</Tooltip>;

  return (
    <div className={styles.display_inline}>
      <OverlayTrigger placement={tooltipPlacement} overlay={tooltip}>
        <span>
          <WarningIcon customStyle={customStyle} />
        </span>
      </OverlayTrigger>
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
    <i
      className={`fa fa-exclamation-triangle ${styles.warningSymbol} ${customStyle}`}
      aria-hidden="true"
    />
  );
};

export default WarningSymbol;
