import React from 'react';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';

import { Icon } from '../../UIKit/atoms';
import styles from './WarningSymbol.scss';

const WarningSymbol = ({
  tooltipText,
  tooltipPlacement = 'right',
  customStyle = null,
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

export const WarningIcon = ({ customStyle }) => {
  return (
    <Icon
      type="warning"
      className={`${styles.warningSymbol} ${customStyle ? customStyle : ''}`}
    />
  );
};

export default WarningSymbol;
