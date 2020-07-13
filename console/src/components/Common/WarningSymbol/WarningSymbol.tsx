import React from 'react';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';

import { Icon } from '../../UIKit/atoms';
import styles from '../Common.scss';

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
        <WarningIcon customStyle={customStyle} />
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
  return <Icon type="warning" color="red.primary" className={customStyle} />;
};

export default WarningSymbol;
