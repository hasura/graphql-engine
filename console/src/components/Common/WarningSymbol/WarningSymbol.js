import React from 'react';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';

import styles from './WarningSymbol.scss';

const WarningSymbol = ({
  tooltipText,
  tooltipPlacement = 'right',
  customStyle = null,
}) => {
  const tooltip = <Tooltip>{tooltipText}</Tooltip>;

  return (
    <div className={styles.display_inline}>
      <OverlayTrigger placement={tooltipPlacement} overlay={tooltip}>
        <i
          className={`fa fa-exclamation-triangle ${styles.warningSymbol} ${
            customStyle ? customStyle : ''
          }`}
          aria-hidden="true"
        />
      </OverlayTrigger>
    </div>
  );
};

export default WarningSymbol;
